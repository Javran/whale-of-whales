{-# LANGUAGE
    RecordWildCards
  , TypeApplications
  , OverloadedStrings
  #-}
module Javran.Wow.Env
  ( getWEnvFromSys
  ) where

import System.Environment
import Text.ParserCombinators.ReadP
import Data.String
import Data.Int
import Web.Telegram.API.Bot
import qualified Data.Text as T
import Data.Char

import Javran.Wow.Types

getWEnvFromSys :: IO WEnv
getWEnvFromSys = do
    botToken <- Token . fromString <$> getEnv "BOT_TOKEN"
    pullTimeout <- read @Int <$> getEnv "PULL_TIMEOUT"
    kickTimeout <- read @Int <$> getEnv "KICK_TIMEOUT"
    errFile <- getEnv "ERR_FILE"
    stateFile <- getEnv "STATE_FILE"
    watchingGroups <- parseChatIds <$> getEnv "WATCHING_GROUPS"
    whaleStickers <- parseStickers <$> getEnv "WHALE_STICKERS"
    pure (WEnv {..})

parseStickers :: String -> [T.Text]
parseStickers raw = case readP_to_S parser raw of
    [(xs, [])] -> T.pack <$> xs
    _ -> error "error while parsing WHALE_STICKERS"
  where
    parser :: ReadP [String]
    parser =
      skipSpaces *>
      ((munch1 (\x -> not (isSpace x) && x /= ',') <* skipSpaces) `sepBy` (char ',' <* skipSpaces)) <*
      optional (char ',' <* skipSpaces) <*
      eof

parseChatIds :: String -> [Int64]
parseChatIds raw = case readP_to_S parser raw of
    [(xs, [])] -> xs
    _ -> error "error while parsing WATCHING_GROUPS"
  where
    rInt64 :: ReadP Int64
    rInt64 = readS_to_P reads
    
    parser :: ReadP [Int64]
    parser =
      skipSpaces *>
      ((rInt64 <* skipSpaces) `sepBy` (char ',' <* skipSpaces)) <*
      optional (char ',' <* skipSpaces) <*
      eof
