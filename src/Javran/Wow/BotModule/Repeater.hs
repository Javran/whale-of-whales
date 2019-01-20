{-# LANGUAGE
    NamedFieldPuns
  #-}
module Javran.Wow.BotModule.Repeater
  ( Repeater
  ) where

import Control.Monad.RWS
import Web.Telegram.API.Bot

import Data.Default.Class
import Data.Time.Clock.POSIX
import Data.Time
import Data.Char

import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()
import Javran.Wow.Util

updateToRepeatDigest :: Update -> Maybe (UTCTime, (Int, RepeatDigest))
updateToRepeatDigest upd
  | Update
    { message =
        Just msg@Message
        { date
        , from = Just User {user_id}
        }
    } <- upd
  = do
      let curTime :: UTCTime
          curTime = posixSecondsToUTCTime (fromIntegral date)
      case msg of
        Message {text = Just txt} ->
          let dg = T.filter (\x -> isPrint x && not (isSpace x)) txt
          in pure (curTime, (user_id, RepeatMessageDigest dg))
        Message {sticker = Just Sticker {sticker_file_id}} ->
          pure (curTime, (user_id, RepeatStickerDigest sticker_file_id))
        _ -> Nothing
  | otherwise = Nothing

{-
  Repeater Design:

  - remove outdated repeaterdata when start processing message
  - recognize repeated message (it make sense that we only look at current message
    and find previous messages that is the same, or almost the same)
  - do the repeating and move message into cooldown

  -}

-- should only call this function when
-- we have a sticker or text message
processRepeater :: T.Text -> Update -> WowM ()
processRepeater groupId upd
  | Just digest@(curTime, (_, rd)) <- updateToRepeatDigest upd
  = do
    WEnv
      { repeatWindow
      , repeatCooldown
      , repeatUniqUserCount
      } <- ask
    let timeCut =
          -- conversion will consider UTCTime to be seconds.
          takeWhile (\(t, _) -> floor (curTime `diffUTCTime` t) <= repeatWindow)
        updateRcd =
          M.filter (\t -> floor (curTime `diffUTCTime` t) <= repeatCooldown)
    -- + remove outdated message digests (by cutting at time window)
    -- + remove expired cooldown
    modifyGroupState groupId $
      \gs@GroupState{repeaterDigest = rds, repeaterCooldown = rcd} ->
        gs { repeaterDigest = digest:timeCut rds
           , repeaterCooldown = updateRcd rcd
           }
    GroupState
      { repeaterDigest = newRds
      , repeaterCooldown = newCds
      } <- getGroupState groupId

    let uniqUserCount =
              IS.size
            . foldMap (\(_, (u, rd')) ->
                          if rd == rd'
                            then IS.singleton u
                            else IS.empty)
            $ newRds
    when (rd `M.member` newCds) $
      liftIO $ putStrLn "no repeat due to cooldown"
    when ((rd `M.notMember` newCds) && uniqUserCount >= repeatUniqUserCount) $ do
      -- now we should repeat
      let Update
            { message = Just msg@Message{forward_from, message_id}
            } = upd
          chatId = ChatId (read (T.unpack groupId))
      case forward_from of
        Just _ -> do
          -- forward the message
          -- for reasons we shouldn't forward from original message,
          -- (which will result in "message not found" error, flippng great design choice.)
          -- but from the message we are dealing with.
          let req = ForwardMessageRequest
                      { forward_chat_id = chatId
                      , forward_from_chat_id = chatId
                      , forward_disable_notification = Nothing
                      , forward_message_id = message_id
                      }
          _ <- tryWithTag "RepeatForward" $ liftTC (forwardMessageM req)
          pure ()
        _ ->
          case rd of
            RepeatMessageDigest {} -> do
              let Message {text = Just content} = msg
                  req = def
                          { message_chat_id = chatId
                          , message_text = content
                          }
              _ <- tryWithTag "RepeatText" $ liftTC (sendMessageM req)
              pure ()
            RepeatStickerDigest {} -> do
              let Message {sticker = Just Sticker{sticker_file_id}} = msg
                  req = SendStickerRequest
                          { sticker_chat_id = chatId
                          , sticker_sticker = sticker_file_id
                          , sticker_disable_notification = def
                          , sticker_reply_to_message_id = def
                          , sticker_reply_markup = def
                          }
              _ <- tryWithTag "RepeatSticker" $ liftTC (sendStickerM req)
              pure ()

      modifyGroupState groupId $
        \gs@GroupState{repeaterCooldown = rcd} ->
          gs {repeaterCooldown = M.insert rd curTime rcd}

  | otherwise = pure ()

data Repeater

instance BotModule Repeater where
    -- Repeater will always return False to allow other modules
    -- to do further processing
    bmUpdFulfiller _ = UpdFulfiller $ \upd -> case upd of
        Update {message = Just Message {chat = Chat {chat_id}}}
          -> processRepeater (showText chat_id) upd >> pure False
        _ -> pure False
