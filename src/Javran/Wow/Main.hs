{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  #-}
module Javran.Wow.Main
  ( main
  ) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot
import System.Environment
import Data.String
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Concurrent
import Data.Functor

import Javran.Wow.Env
import Javran.Wow.Worker
import Javran.Wow.Types


{-
  ENV:

  - BOT_TOKEN=<token>
  - (TODO) PULL_TIMEOUT=<in seconds>
  - (TODO) KICK_TIMEOUT=<in seconds>
  - (TODO) STATE_FILE=<filename>
  - (TODO) ERR_FILE=<filename>
 -}
{-
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
-}
botWorker :: WEnv -> IO ()
botWorker wenv@WEnv{..} =
    -- outter forever for continuing from critical errors
    forever $ catch run errHandler
  where
    run :: IO ()
    run = do
      mgr <- newManager tlsManagerSettings
      initState <- loadState
      -- inner forever for update handling
      void $ runWowM wenv initState mgr $ forever $ do
        WState {..} <- getWState
        mapM_ handleUpdate =<< liftTC (do
            void deleteWebhookM
            let req = GetUpdatesRequest
                        { updates_offset = succ <$> lastUpdate
                        , updates_limit = Nothing
                        , updates_timeout = Just pullTimeout
                        , updates_allowed_updates = Nothing
                        }
            Response {..} <- getUpdatesM req
            pure result)
        handleKick
        saveState

    errHandler :: SomeException -> IO ()
    errHandler e =
      appendLogTo errFile $
        "Exception caught: " ++ displayException e

main :: IO ()
main = do
  we <- getWEnvFromSys
  void $ forkIO (botWorker we)

  {-
  mgr <- newManager tlsManagerSettings
  
  
  tok <- getToken
  _ <- runTelegramClient tok mgr $ do

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
-}
