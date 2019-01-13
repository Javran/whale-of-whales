{-# OPTIONS_GHC -fno-warn-orphans #-}
module Javran.Wow.Default where

import Data.Default.Class
import Web.Telegram.API.Bot
import Data.Text (Text)

instance Default Text where
  def = mempty

-- caution: this instance is here just to allow deriving other defaults.
instance Default ChatId where
  def = ChatId 0

instance Default AnswerCallbackQueryRequest
instance Default SendMessageRequest
instance Default InlineKeyboardButton
instance Default GetUpdatesRequest
instance Default DeleteMessageRequest
instance Default a => Default (SendStickerRequest a)
