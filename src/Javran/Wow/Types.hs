{-# LANGUAGE
    ScopedTypeVariables
  , ExplicitForAll
  , NamedFieldPuns
  , StandaloneDeriving
  , DeriveGeneric
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Javran.Wow.Types
  ( UserVerificationMessage(..)
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
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import System.Random
import Data.Default.Class
import GHC.Generics

deriving instance Eq ChatType

data UserVerificationMessage = UserVerificationMessage
  { timestamp :: UTCTime
  , userSet :: IS.IntSet
  } deriving (Read, Show, Eq)

-- "P" for persistent
data WPState = WPState
  { lastUpdate :: Maybe Int
  , pendingKicks :: M.Map (Int, T.Text) UserVerificationMessage
  } deriving (Read, Show, Eq, Generic)

instance Default (M.Map a b) where
  def = M.empty

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
