{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  #-}
module Javran.Wow.BotModule.SendYesOrNo where

import Data.Maybe
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Data.Default.Class

import qualified Data.Text as T

import Javran.Wow.Types
import Javran.Wow.Default ()
import Javran.Wow.Util
import Javran.Wow.Base

data SendYesOrNo

userDesc :: User -> T.Text
userDesc User{..} =
    if T.length lastName > 0
      then firstName <> " " <> lastName
      else firstName
  where
    firstName = user_first_name
    lastName = fromMaybe T.empty user_last_name

yesMessages :: [[T.Text]]
yesMessages =
    [ ["说", "书", "舒"]
    , ["的", "得", "地"]
    , ["对", "队", "兑"]
    ]

noMessages :: [[T.Text]]
noMessages =
    [ ["说", "书", "舒"]
    , ["的", "得", "地"]
    , ["不", "卜", "部"]
    , ["对", "队", "兑"]
    ]

replyWith :: Bool -> Update -> WowM ()
replyWith isYes upd
    | Update
        { message = Just msg@Message {chat = Chat {chat_id}}
        } <- upd
    , Message
        { text = Just content
        , from = Just user
        , reply_to_message
        } <- msg
      -- should trigger when it's replying to some message
    , isJust reply_to_message || T.length content > 2
    = do
        WEnv {selfUserId} <- ask
        -- prioritize on replies, we only try a regular "yes or no" after
        -- pattern matching has failed
        let who =
              case reply_to_message of
                Just Message {from = Just u@User {user_id}} ->
                  if user_id == selfUserId
                    then "我"
                    else userDesc u
                _ -> userDesc user
        rndMsg <- T.concat <$> mapM pickM (if isYes then yesMessages else noMessages)
        let req = def { message_chat_id = ChatId chat_id
                      , message_text = who <> rndMsg <> "!"
                      }
        _ <- tryWithTag "YorN" $ liftTC $ sendMessageM req
        pure ()
    | otherwise = pure ()

processUpdate :: Update -> WowM Bool
processUpdate upd
    | Update { message = Just msg } <- upd
    , Just cmd <- extractBotCommand msg
    , Just yOrN <- case cmd of
        "/y" -> pure True
        "/yes" -> pure True
        "/n" -> pure False
        "/no" -> pure False
        _ -> Nothing
    = replyWith yOrN upd >> pure True
    | otherwise = pure False

instance BotModule SendYesOrNo where
    bmUpdFulfiller _ = UpdFulfiller processUpdate
