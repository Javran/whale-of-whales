{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.Wow.BotModule.Chiba
  ( Chiba
  ) where

import Web.Telegram.API.Bot
import Data.Default.Class
import qualified Data.Text as T

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()
import Javran.Wow.Util

data Chiba

instance BotModule Chiba where
    bmUpdFulfiller _ = UpdFulfiller $ \case
        Update { message = Just msg@Message { chat = Chat {chat_id}, text = Just content } }
            | Just "/chiba" <- extractBotCommand msg
            , "/" `T.isPrefixOf` content ->
                  let req = def { message_chat_id = ChatId chat_id
                                , message_text = "它跟你发个吃吧"
                                }
                  in tryWithTag "Chiba" (liftTC $ sendMessageM req) >> pure True
        _ -> pure False
