{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  #-}

module Javran.Wow.BotModule.Lmgtfy
  ( Lmgtfy
  ) where

import Web.Telegram.API.Bot
import qualified Data.Text as T
import Network.URI.Encode (encodeText)
import Data.Default.Class

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Util
import Javran.Wow.Default ()

data Lmgtfy

instance BotModule Lmgtfy where
    bmUpdFulfiller _ = UpdFulfiller $ \case
        Update { message = Just msg@Message{chat = Chat {chat_id}} }
            | Just (cmd, (beforeTxt', afterTxt')) <- extractBotCommand' msg
            , cmd == "/g" || cmd == "/google"
            , (beforeTxt, afterTxt) <- (T.strip beforeTxt', T.strip afterTxt')
            , T.length beforeTxt > 0 || T.length afterTxt > 0
              -> do
                let bfEnc = encodeText beforeTxt
                    afEnc = encodeText afterTxt
                    searchTxt
                        | T.length beforeTxt == 0 = afEnc
                        | T.length afterTxt == 0 = bfEnc
                        | otherwise = bfEnc <> "+" <> afEnc
                    req = def { message_text = "http://lmgtfy.com/?q=" <> searchTxt
                              , message_chat_id = ChatId chat_id
                              }
                _ <- liftTC $ sendMessageM req
                pure True
        _ -> pure False

