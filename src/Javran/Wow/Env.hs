{-# LANGUAGE
    RecordWildCards
  , TypeApplications
  #-}
module Javran.Wow.Env
  ( mGetWEnv
  ) where

import System.Environment
import Data.String
import System.IO.Memoize (once)
import Web.Telegram.API.Bot

import Javran.Wow.Types

mGetWEnv :: IO (IO WEnv)
mGetWEnv = once $ do
    botToken <- Token . fromString <$> getEnv "BOT_TOKEN"
    pullTimeout <- read @Int <$> getEnv "PULL_TIMEOUT"
    kickTimeout <- read @Int <$> getEnv "KICK_TIMEOUT"
    errFile <- getEnv "ERR_FILE"
    stateFile <- getEnv "STATE_FILE"
    pure (WEnv {..})
