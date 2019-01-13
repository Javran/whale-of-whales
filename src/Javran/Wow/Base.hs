{-# LANGUAGE
    ExplicitForAll
  , TypeApplications
  , NamedFieldPuns
  , ScopedTypeVariables
  #-}
module Javran.Wow.Base
  ( liftTC
  , runWowM
  , logM
  , appendLogTo
  , tryWithTag
  , genNextM

  ) where

import Data.Time
import System.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except
import Web.Telegram.API.Bot
import Servant.Client
import Network.HTTP.Client (Manager)

import Javran.Wow.Types

liftTC :: TelegramClient a -> WowM a
liftTC m = do
  tok <- asks botToken
  lift (runReaderT m tok)

runWowM :: forall a. WEnv -> WState -> Manager -> WowM a -> IO (Either ServantError a)
runWowM we@WEnv {botToken = tok} ws mgr act =
    runClient mTC tok mgr
  where
    mTC :: TelegramClient a
    mTC = ReaderT (pure (fst <$> evalRWST act we ws))

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
    WState {rGen} <- get
    let (r, rGen') = random rGen
    modify (\s -> s{rGen = rGen'})
    pure r

logM :: String -> WowM ()
logM msg = asks errFile >>= \fp -> liftIO (appendLogTo fp msg)

appendLogTo :: FilePath -> String -> IO ()
appendLogTo logPath msg = do
    t <- getZonedTime
    let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
        timeStr = formatTime defaultTimeLocale "%T" t
        header = "[" <> dateStr <> " " <> timeStr <> "]"
    appendFile logPath (header <> " " <> msg <> "\n")
