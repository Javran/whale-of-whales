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
import System.IO

import Javran.Wow.Env
import Javran.Wow.Worker
import Javran.Wow.Types

botWorker :: WEnv -> IO ()
botWorker wenv@WEnv{..} =
    -- outter forever for continuing from critical errors
    forever $ catch run errHandler
  where
    run :: IO ()
    run = do
      mgr <- newManager tlsManagerSettings
      initState <- loadState stateFile
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
  forever $ threadDelay (1000 * 1000 * 1000)
