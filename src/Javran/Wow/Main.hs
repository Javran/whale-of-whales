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
import Control.Monad
import Control.Monad.RWS
import Control.Exception
import Control.Concurrent
import Data.Default.Class

import Javran.Wow.Env
import Javran.Wow.Worker
import Javran.Wow.Types
import Javran.Wow.Base

botWorker :: WEnv -> IO ()
botWorker wenv@WEnv{..} =
    -- outter forever for continuing from critical errors
    forever $ catch run errHandler
  where
    run :: IO ()
    run = do
      putStrLn "bot started"
      mgr <- newManager tlsManagerSettings
      initState <- loadState stateFile
      -- inner forever for update handling
      void $ runWowM wenv initState mgr $ forever $ do
        WState {..} <- get
        mapM_ handleUpdate =<< liftTC (do
            void deleteWebhookM
            let req = def
                        { updates_offset = succ <$> lastUpdate
                        , updates_timeout = Just pullTimeout
                        }
            Response {..} <- getUpdatesM req
            pure result)
        handleKicks
        saveState

    errHandler :: SomeException -> IO ()
    errHandler e =
      appendLogTo errFile $
        "Exception caught: " ++ displayException e

main :: IO ()
main = do
  we <- getWEnvFromSys
  void $ forkIO (botWorker we)
  forever $ threadDelay (1000 * 1000 * 1000)
