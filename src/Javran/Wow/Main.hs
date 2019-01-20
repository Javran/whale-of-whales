{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , MultiWayIf
  , LambdaCase
  #-}
module Javran.Wow.Main
  ( main
  ) where

import Control.Monad
import Control.Concurrent

import System.Environment
  ( getArgs
  )
import System.Exit
  ( exitFailure
  )

import Javran.Wow.ProcessUpdate
import Javran.Wow.Base

main :: IO ()
main =
  getArgs >>= \case
    [configFp] ->
      do
        we <- getWEnv configFp
        void $ forkIO (startBot we 0)
        forever $ threadDelay (1000 * 1000 * 1000)
    _ ->
      putStrLn "wow <config_file.yaml>" >>
      exitFailure
