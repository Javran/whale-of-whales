{-# LANGUAGE
    ScopedTypeVariables
  , ExplicitForAll
  , NamedFieldPuns
  , StandaloneDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Javran.Wow.Types
  ( PendingKick(..)
  , WState(..)
  , WEnv(..)
  , WowM
  ) where

import Data.Int
import Data.Time
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Servant.Client
import qualified Data.Text as T
import System.Random

deriving instance Eq ChatType

data PendingKick = PendingKick
  { groupId :: T.Text
  , userId :: Int
  , timestamp :: UTCTime
  , kickMeta :: T.Text
  } deriving (Read, Show)

data WState = WState
  { lastUpdate :: Maybe Int
    -- TODO: key by user (new join event shouldn't update existing records)
  , pendingKicks :: [PendingKick]
  , rGen :: StdGen
  } deriving (Read, Show)

data WEnv = WEnv
  { botToken :: Token
  , pullTimeout :: Int
  , kickTimeout :: Int
  , errFile :: FilePath
  , stateFile :: FilePath
  , watchingGroups :: [Int64]
  }

type WowM a = RWST WEnv () WState ClientM a

  -- ReaderT WEnv (StateT WState ClientM) a

