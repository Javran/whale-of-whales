{-# OPTIONS_GHC -fno-warn-orphans #-}
module Javran.Wow.Default where

import Data.Default.Class
import Web.Telegram.API.Bot
import qualified Data.Text as T

instance Default T.Text where
  def = T.empty

-- caution: this instance is here just to allow deriving other defaults.
instance Default ChatId where
  def = ChatId 0

instance Default AnswerCallbackQueryRequest

instance Default SendMessageRequest

instance Default InlineKeyboardButton
