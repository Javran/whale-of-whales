{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}
module Javran.WoW.Main
  ( main
  ) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot
import System.Environment
import Data.String
import Control.Monad
import Control.Monad.IO.Class

data PendingKick =
  { channelId :: Int
  , userId :: Int
  , timestamp :: Int
  }

data WState = WS
  { lastUpdate :: Maybe Int
  , pendingKicks :: [PendingKick]
  }

{-
  ENV:

  - BOT_TOKEN=<token>
  - (TODO) PULL_TIMEOUT=<in seconds>
  - (TODO) KICK_TIMEOUT=<in seconds>
  - (TODO) STATE_FILE=<filename>
  - (TODO) ERR_FILE=<filename>
 -}

getToken :: IO Token
getToken = Token . fromString <$> getEnv "BOT_TOKEN"

handleUpdateM :: Update -> TelegramClient MessageResponse
handleUpdateM Update {message = Just msg} = sendMessageM req
  where
    Message {message_id, chat = Chat {chat_id}} = msg
    req =
        SendMessageRequest
        { message_chat_id = ChatId chat_id
        , message_text = "whales!"
        , message_parse_mode = Nothing
        , message_disable_web_page_preview = Nothing
        , message_disable_notification = Nothing
        , message_reply_to_message_id = Just message_id
        , message_reply_markup = Nothing
        }

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  tok <- getToken
  _ <- runTelegramClient tok mgr $ do
    -- make sure that we are NOT using webhook
    _ <- deleteWebhookM
    let req = GetUpdatesRequest Nothing Nothing (Just 5) Nothing
    Response {result = resps, parameters = ps} <- getUpdatesM req
    liftIO $ (putStrLn "params:" >> print ps)
    forM_ resps $ \resp -> do
      liftIO $ do
        putStrLn "response:"
        print resp
      handleUpdateM resp
  pure ()
