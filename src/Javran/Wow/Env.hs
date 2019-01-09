{-# LANGUAGE
    RecordWildCards
  , TypeApplications
  #-}
module Javran.Wow.Env
  ( getWEnvFromSys
  ) where

import System.Environment
import Data.String
import Web.Telegram.API.Bot

import Javran.Wow.Types

getWEnvFromSys :: IO WEnv
getWEnvFromSys = do
    botToken <- Token . fromString <$> getEnv "BOT_TOKEN"
    pullTimeout <- read @Int <$> getEnv "PULL_TIMEOUT"
    kickTimeout <- read @Int <$> getEnv "KICK_TIMEOUT"
    errFile <- getEnv "ERR_FILE"
    stateFile <- getEnv "STATE_FILE"
    pure (WEnv {..})
