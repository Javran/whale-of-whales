{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  #-}
module Javran.Wow.BotModule.CommandSink
  ( CommandSink
  ) where

{-
  this bot module simply captures all commands
  and stop further processing on them.

  obviously we should only use this module
  when all modules following it expect no commands at all.
 -}

import Control.Monad.RWS
import Web.Telegram.API.Bot

import qualified Data.Text as T

import Javran.Wow.Types
import Javran.Wow.Util

data CommandSink

instance BotModule CommandSink where
    bmUpdFulfiller _ = UpdFulfiller $ \case
        Update { message = Just msg }
            | Just cmd <- extractBotCommand msg -> do
                liftIO $ putStrLn $ "unrecognized command: " ++ T.unpack cmd
                pure True
        _ -> pure False
