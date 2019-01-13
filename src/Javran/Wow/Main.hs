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
import Control.Monad.Catch
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
      void $ runWowM wenv initState mgr $ do
        -- we need to do this only once at startup
        _ <- tryWithTag "DelWebhook" $ liftTC deleteWebhookM
        -- inner forever for update handling        
        forever $ do
          (_oldSt@WPState {..}, _) <- get
          {-
            INVARIANT:
              - handleUpdate should be able to capture all ServantError inside of it.
              - same invariant for handleKicks
           -}
          mapM_ handleUpdate =<< liftTC (do
            let req = def
                      { updates_offset = succ <$> lastUpdate
                      , updates_timeout = Just pullTimeout
                      }
            Response {..} <- getUpdatesM req
            pure result)
          handleKicks
          -- TODO: diff-then-write
          -- newSt <- get
          -- when (oldSt /= newSt) saveState
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
