{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.Wow.BotModule.BianTaiXue
  ( BianTaiXue
  ) where

import Control.Monad.RWS
import Web.Telegram.API.Bot
import Text.ParserCombinators.ReadP
import Data.Default.Class

import qualified Data.Text as T
import Data.Char
import Data.Maybe

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()

data BianTaiXue

getBtxMsg :: T.Text -> Maybe T.Text
getBtxMsg raw = case readP_to_S btxMsgP (T.unpack raw) of
    [(r, [])] -> Just (T.pack r)
    _ -> Nothing
  where
    oneOfChar :: [] Char -> ReadP Char
    oneOfChar xs = satisfy (`elem` xs)

    btxMsgP :: ReadP String
    btxMsgP = do
      skipSpaces
      _ <- char '我'
      m <- option Nothing $ do
        u <- option [] $ (:[]) <$> oneOfChar "就救"
        v <- oneOfChar "是世"
        pure (Just $ u <> [v])
      b <- oneOfChar "变變変"
      c <- oneOfChar "态態"
      d <- oneOfChar "学學"
      ys <- munch1 (not . isSpace)
      skipSpaces
      eof
      pure ('他': fromMaybe [] m <> [b,c,d] <> ys)

instance BotModule BianTaiXue where
    bmUpdFulfiller _ = UpdFulfiller $ \case
        Update { message = Just Message { chat = Chat {chat_id}, text = Just content} }
          | Just respContent <- getBtxMsg content -> do
              roll <- genNextRM (0,99 :: Int)
              if roll < 80
                then do
                  let req = def { message_chat_id = ChatId chat_id
                                , message_text = respContent
                                }
                  void $ tryWithTag "Btx" $ liftTC $ sendMessageM req
                  pure True
                else
                  pure False
        _ -> pure False
