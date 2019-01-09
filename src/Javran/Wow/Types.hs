module Javran.Wow.Types
  ( PendingKick(..)
  , WState(..)
  , WEnv(..)
  , WowM
  , liftTC
  , asksWEnv
  , modifyWState
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Web.Telegram.API.Bot
import Servant.Client

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

type WowM a = ReaderT WEnv (StateT WState ClientM) a

liftTC :: (a -> TelegramClient b) -> (a -> WowM b)
liftTC act x = do
  tok <- asks botToken
  lift (lift (runReaderT (act x) tok))

asksWEnv :: (WEnv -> r) -> WowM r
asksWEnv = asks

modifyWState :: (WState -> WState) -> WowM ()
modifyWState = lift . modify
