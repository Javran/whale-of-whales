{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  , MultiWayIf
  , LambdaCase
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
import System.Environment
import System.Exit

import Javran.Wow.Env
import Javran.Wow.ProcessUpdate
import Javran.Wow.Types
import Javran.Wow.Base

{-
  TODO:
  - json for bot config and state serialization
  - or perhaps yaml since it's still closely related in some sense.
 -}

botWorker :: WEnv -> Int -> IO ()
botWorker wenv@WEnv{..} = fix $ \r errCount ->
    if errCount < 10
      then catch (forever run) errHandler >> r (succ errCount)
      else putStrLn "Too many errors, aborting."
  where
    run :: IO ()
    run = do
      putStrLn "bot started"
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
          processKicks
          (newSt, _) <- get
          when (oldSt /= newSt) saveState

    errHandler :: SomeException -> IO ()
    errHandler e =
      appendLogTo logFile $
        "Exception caught: " ++ displayException e

main :: IO ()
main =
  getArgs >>= \case
    [configFp] ->
      do
        we <- getWEnv configFp
        void $ forkIO (botWorker we 0)
        forever $ threadDelay (1000 * 1000 * 1000)
    _ ->
      putStrLn "wow <config_file.yaml>" >>
      exitFailure
