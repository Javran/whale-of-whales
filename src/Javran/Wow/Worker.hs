{-# LANGUAGE RecordWildCards, NamedFieldPuns, StandaloneDeriving #-}
module Javran.Wow.Worker
  ( handleUpdate
  , handleKick
  , loadState
  , saveState
  ) where

import System.IO
import Control.Monad.IO.Class
import Control.Exception
import Web.Telegram.API.Bot

import Javran.Wow.Types

-- TODO: move to Types when done
deriving instance Eq ChatType

handleUpdate :: Update -> WowM ()
handleUpdate u@Update{..} = do
    -- update last seen id
    let upd oldId = Just $ if oldId < update_id then update_id else oldId
    modifyWState (\s@WState{lastUpdate} -> s {lastUpdate = maybe (Just update_id) upd lastUpdate})
    case message of
      Just Message {chat = Chat {chat_type = ct}}
        | ct == Group || ct == Supergroup -> do
            liftIO $ putStrLn $ ">> proc: received: " ++ show u
      _ -> do
        liftIO $ putStrLn $ "-- ignored: revceived: " ++ show u
        pure ()

handleKick :: WowM ()
handleKick =
  -- TODO
  pure ()

loadState :: FilePath -> IO WState
loadState fp =
    catch (read <$> readFile fp) errHandler
  where
    errHandler :: SomeException -> IO WState
    errHandler e = do
      hPutStrLn stderr $ "Exception caught: " ++ displayException e
      pure (WState Nothing [])

saveState :: WowM ()
saveState = do
    WEnv {..} <- asksWEnv id
    st <- getWState
    liftIO $ writeFile stateFile (show st)
