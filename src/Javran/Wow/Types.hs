{-# LANGUAGE
    ScopedTypeVariables
  , ExplicitForAll
  , NamedFieldPuns
  , StandaloneDeriving
  , DeriveGeneric
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Javran.Wow.Types
  ( PendingKick(..)
  , WState
  , WPState(..)
  , WEnv(..)
  , WowM
  ) where

import Data.Int
import Data.Time
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Servant.Client
import qualified Data.Text as T
import System.Random
import Data.Default.Class
import GHC.Generics

deriving instance Eq ChatType

data PendingKick = PendingKick
  { groupId :: T.Text
  , userId :: Int
  , timestamp :: UTCTime
  , kickMeta :: T.Text
  } deriving (Read, Show, Eq)

-- "P" for persistent
data WPState = WPState
  { lastUpdate :: Maybe Int
    -- TODO: key by user (new join event shouldn't update existing records)
  , pendingKicks :: [PendingKick]
  } deriving (Read, Show, Eq, Generic)

instance Default WPState

-- separation of persistent state vs. runtime-only ones
type WState = (WPState, StdGen)

data WEnv = WEnv
  { botToken :: Token
  , pullTimeout :: Int
  , kickTimeout :: Int
  , errFile :: FilePath
  , stateFile :: FilePath
  , watchingGroups :: [Int64]
  }

type WowM a = RWST WEnv () WState ClientM a
