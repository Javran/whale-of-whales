{-# LANGUAGE ScopedTypeVariables, ExplicitForAll, TypeApplications, LambdaCase #-}
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
  , tryWithTag
  ) where

import Data.Int
import Data.Time
import Control.Monad.Reader
import Control.Monad.State
import Web.Telegram.API.Bot
import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Data.Text as T
import qualified Control.Monad.Catch as MCatch

data PendingKick = PendingKick
  { channelId :: T.Text
  , userId :: Int
  , timestamp :: UTCTime
  -- , nonce :: T.Text - could be UUID, as long as option is prefixed with that nonce, we are good to cancel kicking.
  } deriving (Read, Show)

data WState = WState
  { lastUpdate :: Maybe Int
    -- TODO: key by user (new join event shouldn't update existing records)
  , pendingKicks :: [PendingKick]
  } deriving (Read, Show)

data WEnv = WEnv
  { botToken :: Token
  , pullTimeout :: Int
  , kickTimeout :: Int
  , errFile :: FilePath
  , stateFile :: FilePath
  , watchingGroups :: [Int64]
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

-- for not well explained reason, this is not working -
-- the flow of computation just terminates and not a single catch function
-- does their job, which is flipping great
tryWithTag :: String -> WowM a -> WowM (Maybe a)
tryWithTag tag m =
  MCatch.try @_ @ServantError m >>= \case
    Left e -> do
      logM $ "[" ++ tag ++ "] " ++ MCatch.displayException e
      pure Nothing
    Right r -> pure (Just r)

logM :: String -> WowM ()
logM msg = asksWEnv errFile >>= \fp -> liftIO (appendLogTo fp msg)

appendLogTo :: FilePath -> String -> IO ()
appendLogTo logPath msg = do
    t <- getZonedTime
    let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
        timeStr = formatTime defaultTimeLocale "%T" t
        header = "[" <> dateStr <> " " <> timeStr <> "]"
    appendFile logPath (header <> " " <> msg <> "\n")
