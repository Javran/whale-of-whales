{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  , MultiWayIf
  , ScopedTypeVariables
  , LambdaCase
  , ExistentialQuantification
  #-}
module Javran.Wow.ProcessUpdate
  ( processUpdate
  , loadState
  , saveState
  , startBot
  ) where

import Control.Monad.IO.Class
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Data.Time
import Data.Char

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Int
import Control.Exception

import Data.Default.Class
import Data.Proxy
import Data.Time.Clock.POSIX
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Javran.Wow.Base
import Javran.Wow.Default ()
import Javran.Wow.Types
import Javran.Wow.BotModule.UserVerification (UserVerification)
import Javran.Wow.BotModule.SendWhale (SendWhale)
import Javran.Wow.BotModule.SendYesOrNo (SendYesOrNo)
import Javran.Wow.BotModule.CommandSink (CommandSink)

data BMod = forall bm. BotModule bm => BMod (Proxy bm)

-- list of modules
-- note that both updates and post-processings are handled in the same order
botMods :: [BMod]
botMods =
    [ BMod (Proxy :: Proxy UserVerification)
    , BMod (Proxy :: Proxy SendWhale)
    , BMod (Proxy :: Proxy SendYesOrNo)
      -- NOTE: CommandSink consumes all commands,
      -- no bot command modules should be placed after it.
    -- , BMod (Proxy :: Proxy CommandSink)
    ]

int64ToT :: Int64 -> T.Text
int64ToT = T.pack . show

startBot :: WEnv -> Int -> IO ()
startBot wenv@WEnv{..} = fix $ \r errCount ->
    if errCount < 10
      then
        putStrLn ("Starting bot at attempt #" ++ show errCount) >>
        catch (forever run) errHandler >> r (succ errCount)
      else putStrLn "Too many errors, aborting."
  where
    run :: IO ()
    run = do
      putStrLn "Bot started"
      mgr <- newManager tlsManagerSettings
      initState <- loadState stateFile
      void $ runWowM wenv initState mgr $ do
        -- we need to do this only once at startup
        _ <- tryWithTag "DelWebhook" $ liftTC deleteWebhookM
        -- forever for update handling
        forever $ do
          (oldSt@WPState {..}, _) <- get
          {-
            INVARIANT:
              - handleUpdate should be able to capture all ServantError inside of it.
              - same invariant for handleKicks
           -}
          mapM_ processUpdate =<< liftTC (do
            let req = def
                      { updates_offset = succ <$> lastUpdate
                      , updates_timeout = Just pullTimeout
                      }
            Response {..} <- getUpdatesM req
            pure result)
          mapM_ (\(BMod bm) -> bmPostProcess bm) botMods
          (newSt, _) <- get
          when (oldSt /= newSt) saveState

    errHandler :: SomeException -> IO ()
    errHandler e =
      appendLogTo logFile $
        "Exception caught: " ++ displayException e

bumpLastSeen :: Update -> WowM ()
bumpLastSeen Update{..} = do
    -- update last seen id
    let updF oldId = Just $ if oldId < update_id then update_id else oldId
    modify (\(s@WPState{lastUpdate},rg) ->
              (s {lastUpdate = maybe (Just update_id) updF lastUpdate}, rg))

getGroupState :: T.Text -> WowM GroupState
getGroupState chatId =
  gets $ fromMaybe def . M.lookup chatId . groupStates . fst

modifyGroupState :: T.Text -> (GroupState -> GroupState) -> WowM ()
modifyGroupState chatId f =
    modify (\(s@WPState{groupStates = gss},rg) ->
              (s {groupStates = M.alter f' chatId gss}, rg))
  where
    f' Nothing = Just (f def)
    f' (Just x) = Just (f x)

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
    window <- asks repeatWindow
    cd <- asks repeatCooldown
    let timeCut =
          -- conversion will consider UTCTime to be seconds.
          takeWhile (\(t, _) -> floor (curTime `diffUTCTime` t) <= window)
        updateRcd =
          M.filter (\t -> floor (curTime `diffUTCTime` t) <= cd)
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

    let distinctUserCount =
              IS.size
            . foldMap (\(_, (u, rd')) ->
                          if rd == rd'
                            then IS.singleton u
                            else IS.empty)
            $ newRds
    when (rd `M.member` newCds) $
      liftIO $ putStrLn "no repeat due to cooldown"
    when ((rd `M.notMember` newCds) && distinctUserCount >= 3) $ do
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

shouldProcessUpdate :: S.Set Int64 -> Update -> Bool
shouldProcessUpdate wg = \case
  Update
    { message = Just Message {chat = Chat {chat_id}}
    } ->
    chat_id `elem` wg
  Update
    { callback_query =
        Just CallbackQuery
          { cq_message = Just Message {chat = Chat {chat_id}} }
    } ->
    chat_id `elem` wg
  _ -> False

processUpdate :: Update -> WowM ()
processUpdate upd@Update{..} = do
    bumpLastSeen upd
    shouldProcess <- asks $
      \WEnv{watchingGroups=wg} ->
        shouldProcessUpdate wg upd
    when shouldProcess $ do
      let combinedUpdateProcessor =
            getUpdFulfiller (foldMap (\(BMod m) -> bmUpdFulfiller m) botMods)

      -- first intercept by new bot modules
      processed <- combinedUpdateProcessor upd
      case upd of
        Update
          { message =
              Just Message
                { sticker = Just Sticker {sticker_file_id}
                , chat = Chat {chat_id}
                }
          } | not processed -> do
              liftIO $ putStrLn $ "sticker received: " ++ show sticker_file_id
              liftIO $ putStrLn $ "[sticker] " ++ show upd
              processRepeater (int64ToT chat_id) upd
        Update
          { message =
            Just Message
                 { chat = Chat {chat_id = ci}
                 }
          } | not processed -> do
          liftIO $ putStrLn $ "[msg] " ++ show upd
          processRepeater (int64ToT ci) upd
        _ | not processed -> do
          let dbg = False
          when dbg $ liftIO $ putStrLn $ "-- ignored: revceived: " ++ show upd
        _ -> pure ()
