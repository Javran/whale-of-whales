{-# LANGUAGE
    ScopedTypeVariables
  , ExplicitForAll
  , TypeApplications
  , NamedFieldPuns
  , StandaloneDeriving
  #-}
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
  , genNextM
  ) where

import Data.Int
import Data.Time
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Web.Telegram.API.Bot
import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Data.Text as T
-- import qualified Control.Monad.Catch as MCatch
import System.Random

-- TODO: move to Types when done
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


{-
  note that we do really want to capture "trapped errors",
  which means making use of "MonadError ServantError m" instance
  rather than using "MatchCatch", which is meant for "untrapped errors",
  or in other words, those errors that isn't expected by,
  or is not responsible to be handled by ClientM monad.
 -}
tryWithTag :: String -> WowM a -> WowM (Maybe a)
tryWithTag tag m = catchError @ServantError (Just <$> m) $ \e -> do
    logM $ "[" ++ tag ++ "] " ++ show e
    pure Nothing

genNextM :: Random v => WowM v
genNextM = do
    WState {rGen} <- getWState
    let (r, rGen') = random rGen
    modifyWState (\s -> s{rGen = rGen'})
    pure r

logM :: String -> WowM ()
logM msg = asksWEnv errFile >>= \fp -> liftIO (appendLogTo fp msg)

appendLogTo :: FilePath -> String -> IO ()
appendLogTo logPath msg = do
    t <- getZonedTime
    let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
        timeStr = formatTime defaultTimeLocale "%T" t
        header = "[" <> dateStr <> " " <> timeStr <> "]"
    appendFile logPath (header <> " " <> msg <> "\n")
