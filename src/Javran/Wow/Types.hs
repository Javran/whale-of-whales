module Javran.Wow.Types where

import Web.Telegram.API.Bot

data PendingKick = PendingKick
  { channelId :: Int
  , userId :: Int
  , timestamp :: Int
  }

data WState = WState
  { lastUpdate :: Maybe Int
  , pendingKicks :: [PendingKick]
  }

data WEnv = WEnv
  { botToken :: Token
  , pullTimeout :: Int
  , kickTimeout :: Int
  , errFile :: FilePath
  , stateFile :: FilePath
  }

