{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
module Javran.Wow.Types
  ( PendingKick(..)
  , WState(..)
  , WEnv(..)
  , WowM
  , liftTC
  , asksWEnv
  , modifyWState
  , getWState
  , runWowM
  , logM
  , appendLogTo
  ) where

import Data.Time
import Control.Monad.Reader
import Control.Monad.State
import Web.Telegram.API.Bot
import Servant.Client
import Network.HTTP.Client (Manager)

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

liftTC :: TelegramClient a -> WowM a
liftTC m = do
  tok <- asks botToken
  lift (lift (runReaderT m tok))

asksWEnv :: (WEnv -> r) -> WowM r
asksWEnv = asks

modifyWState :: (WState -> WState) -> WowM ()
modifyWState = lift . modify

getWState :: WowM WState
getWState = lift get

runWowM :: forall a. WEnv -> WState -> Manager -> WowM a -> IO (Either ServantError a)
runWowM we@WEnv {botToken = tok} ws mgr act =
    runClient mTC tok mgr
  where
    mTC :: TelegramClient a
    mTC = ReaderT (const (evalStateT (runReaderT act we) ws))

logM :: String -> WowM ()
logM msg = asksWEnv errFile >>= \fp -> liftIO (appendLogTo fp msg)

appendLogTo :: FilePath -> String -> IO ()
appendLogTo logPath msg = do
  t <- getZonedTime
  let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
      timeStr = formatTime defaultTimeLocale "%T" t
      header = "[" <> dateStr <> " " <> timeStr <> "]"
  appendFile logPath (header <> " " <> msg <> "\n")
