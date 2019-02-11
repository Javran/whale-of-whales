{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , TypeApplications
  , OverloadedStrings
  #-}
module Javran.Wow.BotModule.SendWhale
  ( SendWhale
  ) where

import Control.Monad.RWS
import Data.Default.Class
import Data.Int (Int64)
import Text.ParserCombinators.ReadP hiding (get)
import Web.Telegram.API.Bot

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
    msg <- tryWithTag "Whale" $ liftTC $
         sendStickerM ((def @(SendStickerRequest T.Text))
                        { sticker_chat_id = ChatId chatId
                        , sticker_sticker = whaleSticker :: T.Text
                        })
    case msg of
      Just Response {result = Message {from = Just User{user_id}}} ->
        liftIO $ putStrLn $ "[dbg] my user id is: " ++ show user_id
      _ -> pure ()

instance BotModule SendWhale where
    bmUpdFulfiller _ = UpdFulfiller $ \case
        Update { message = Just msg@Message { chat = Chat {chat_id}, text = Just content} }
            | Just cmd <- extractBotCommand msg, isWhaleCommand cmd -> do
                if "/" `T.isPrefixOf` content
                  then do
                    roll <- genNextRM (0,4)
                    if roll == (0 :: Int)
                      then
                        let req = def { message_chat_id = ChatId chat_id
                                      , message_text = "我跟你发个吃吧"
                                      }
                        in void $ tryWithTag "Whale'" $ liftTC $ sendMessageM req
                      else sendWhale chat_id
                  else sendWhale chat_id
                pure True
        _ -> pure False
