{-# LANGUAGE
    RecordWildCards
  , TypeApplications
  , OverloadedStrings
  #-}
module Javran.Wow.Env
  ( getWEnv
  ) where

import System.Environment
import Text.ParserCombinators.ReadP
import Data.String
import Data.Int
import Web.Telegram.API.Bot
import qualified Data.Text as T
import Data.Char
import qualified Data.Yaml as Yaml

import Javran.Wow.Types

getWEnv :: FilePath -> IO WEnv
getWEnv = Yaml.decodeFileThrow

parseStickers :: String -> [T.Text]
parseStickers raw = case readP_to_S parser raw of
    [(xs, [])] -> T.pack <$> xs
    _ -> error "error while parsing WHALE_STICKERS"
  where
    parser :: ReadP [String]
    parser =
      skipSpaces *>
      (
        (munch1 (\x -> not (isSpace x) && x /= ',') <* skipSpaces)
        `sepBy`
        (char ',' <* skipSpaces)
      ) <*
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
