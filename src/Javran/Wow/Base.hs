{-# LANGUAGE
    ExplicitForAll
  , TypeApplications
  , NamedFieldPuns
  , ScopedTypeVariables
  , TupleSections
  , RecordWildCards
  #-}
module Javran.Wow.Base
  ( liftTC
  , runWowM
  , logM
  , appendLogTo
  , tryWithTag
  , genNextM
  , genNextRM
  , pickM
  , loadState
  , saveState
  , getWEnv
  ) where

import Data.Time
import System.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except
import Web.Telegram.API.Bot
import Servant.Client
import Control.Exception
import System.IO
import Network.HTTP.Client (Manager)
import Data.Default.Class
import qualified Data.Yaml as Yaml
import Data.List

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
    (_, rGen) <- get
    let (r, rGen') = random rGen
    modify (\(p, _) -> (p, rGen'))
    pure r

genNextRM :: Random v => (v, v) -> WowM v
genNextRM range = do
    (_, rGen) <- get
    let (r, rGen') = randomR range rGen
    modify (\(p, _) -> (p, rGen'))
    pure r

-- randomly pick one, must not be empty
pickM :: [a] -> WowM a
pickM [] = fail "pickM: empty list"
pickM xs = do
    let l = length xs
    ind <- genNextRM (0, l-1)
    pure $ xs !! ind

logM :: String -> WowM ()
logM msg = asks logFile >>= \fp -> liftIO (appendLogTo fp msg)

appendLogTo :: FilePath -> String -> IO ()
appendLogTo logPath msg = do
    t <- getZonedTime
    let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
        timeStr = formatTime defaultTimeLocale "%T" t
        header = "[" <> dateStr <> " " <> timeStr <> "]"
    appendFile logPath (header <> " " <> msg <> "\n")


-- stolen from:
-- https://github.com/snoyberg/yaml/blob/35f0286d83acf6c27e00cf8edbfc43c841109760/yaml/test/Data/Yaml/IncludeSpec.hs#L131-L134
isYamlFileNotFoundException :: Yaml.ParseException -> Bool
isYamlFileNotFoundException (Yaml.InvalidYaml (Just (Yaml.YamlException msg)))
  | "Yaml file not found: " `isPrefixOf` msg = True
isYamlFileNotFoundException _ = False

loadState :: FilePath -> IO WState
loadState fp =
    catch load errHandler
  where
    load :: IO WState
    load = do
      ps <- Yaml.decodeFileThrow fp
      g <- newStdGen
      pure (ps,g)
    
    errHandler :: SomeException -> IO WState
    errHandler e
      | Just pe <- fromException @Yaml.ParseException e
      , isYamlFileNotFoundException pe = do
          hPutStrLn stderr "State file does not exist, assuming fresh start."
          (def,) <$> newStdGen
      | otherwise = do
          hPutStrLn stderr $ "Exception caught: " ++ displayException e
          (def,) <$> newStdGen

saveState :: WowM ()
saveState = do
    WEnv {..} <- ask
    (st, _) <- get
    liftIO $ Yaml.encodeFile stateFile st

getWEnv :: FilePath -> IO WEnv
getWEnv = Yaml.decodeFileThrow
