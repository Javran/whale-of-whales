{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.Wow.Util
  ( extractBotCommand
  , userDesc
  , showText
  ) where

import qualified Data.Text as T
import Web.Telegram.API.Bot
import Data.Maybe
import Data.Char

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

-- try to figure out how to describe this user in chat.
userDesc :: User -> T.Text
userDesc
    User
      { user_first_name
      , user_last_name
      , user_username
      , user_id
      } =
    if T.all (\c -> isSpace c || not (isPrint c)) attempt
      then
        -- attempt #2: describe by username,
        -- if it exists, it won't be empty (at least 5 characters)
        fromMaybe
          -- last resort: user id, which guarantees to exist
          ("UID:" <> T.pack (show user_id))
          user_username
      else attempt
  where
    firstName = user_first_name
    lastName = fromMaybe T.empty user_last_name
    -- attempt #1: describe by first name then optionally last name
    attempt = if T.length lastName > 0
      then firstName <> " " <> lastName
      else firstName

showText :: Show a => a -> T.Text
showText = T.pack . show
