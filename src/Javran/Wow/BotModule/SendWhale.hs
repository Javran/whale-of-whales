{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , TypeApplications
  #-}
module Javran.Wow.BotModule.SendWhale
  ( SendWhale
  ) where

import Data.Int (Int64)
import Text.ParserCombinators.ReadP hiding (get)
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Data.Default.Class

import qualified Data.Text as T

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()
import Javran.Wow.Util

isWhaleCommand :: T.Text -> Bool
isWhaleCommand inp = case readP_to_S pWhale (T.unpack inp) of
    [(_,"")] -> True
    _ -> False
  where
    {-
      examples:

      - /whale
      - /whales
      - /wwwwwwwhales
      - /wwwwhales
     -}
    pWhale =
      char '/' *>
      munch1 (== 'w') *>
      string "hale" *>
      optional (char 's') *>
      eof

data SendWhale

sendWhale :: Int64 -> WowM ()
sendWhale chatId = do
    WEnv {whaleStickers} <- ask
    whaleSticker <- pickM whaleStickers
    void $ tryWithTag "Whale" $ liftTC $
         sendStickerM ((def @(SendStickerRequest T.Text))
                        { sticker_chat_id = ChatId chatId
                        , sticker_sticker = whaleSticker :: T.Text
                        })

instance BotModule SendWhale where
    bmUpdFulfiller _ = UpdFulfiller $ \case
        Update { message = Just msg@Message { chat = Chat {chat_id}} }
            | Just cmd <- extractBotCommand msg, isWhaleCommand cmd ->
                sendWhale chat_id >> pure True
        _ -> pure False
