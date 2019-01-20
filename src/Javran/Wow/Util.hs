{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.Wow.Util
  ( extractBotCommand
  ) where

import qualified Data.Text as T
import Web.Telegram.API.Bot

{-

  extract and normalize bot command from a message

  - command always begins with "/"
  - it seems that telegram only recognize command with length <= 64 excluding "/"
    so the length of return text is at maximum 65
  - by normalize I mean converting to lowercase

 -}
extractBotCommand :: Message -> Maybe T.Text
extractBotCommand msg
    | Message { entities = Just es, text = Just content } <- msg
    , MessageEntity {me_offset, me_length}:_ <-
        filter (\m -> me_type m == "bot_command") es
    = Just (T.toLower . T.take me_length . T.drop me_offset $ content)
    | otherwise = Nothing
