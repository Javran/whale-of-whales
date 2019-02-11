{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  , TypeApplications
  #-}
module Javran.Wow.BotModule.SimpleRandom
  ( SimpleRandom
  ) where

import Web.Telegram.API.Bot
import Data.Default.Class
import Control.Monad.RWS
import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()
import Javran.Wow.Util
import Text.ParserCombinators.ReadP
import Data.Char

import qualified Data.Text as T

rollP :: ReadP (Integer, Integer)
rollP =
    skipSpaces *>
    ((,) <$> (intP <* skipSpaces)
         <*> (intP <* skipSpaces)) <* eof
  where
    intP = readS_to_P (reads @Integer)

pickP :: ReadP [String]
pickP =
    skipSpaces *>
    munch1 (not . isSpace) `sepBy1` skipSpaces
    <* skipSpaces <* eof

data SimpleRandom

replyMsgWithReq :: String -> Message -> (SendMessageRequest -> SendMessageRequest) -> WowM ()
replyMsgWithReq tag msg modifyReq =
    void $ tryWithTag tag (liftTC $ sendMessageM req)
  where
    Message {message_id, chat = Chat {chat_id}} = msg
    req = modifyReq $
            def { message_chat_id = ChatId chat_id
                , message_reply_to_message_id = Just message_id
                }

handleRoll :: Message -> T.Text -> WowM ()
handleRoll msg raw = case readP_to_S rollP (T.unpack raw) of
  [((a,b), "")] -> do
      v <- genNextRM (if a <= b then (a,b) else (b,a))
      replyMsgWithReq "Roll" msg $
          \req -> req {message_text = T.pack (show v)}
  _ ->
    replyMsgWithReq "Roll" msg $
          \req -> req { message_parse_mode = Just Markdown
                      , message_text = "`/roll <int> <int>`"
                      }

handlePick :: Message -> T.Text -> WowM ()
handlePick msg raw = case readP_to_S pickP (T.unpack raw) of
  [(xs, "")] -> do
      let l = length xs
      ind <- genNextRM (0, l-1)
      replyMsgWithReq "Pick" msg $
          \req -> req {message_text = T.pack (xs !! ind)}
  _ ->
    replyMsgWithReq "Pick" msg $
          \req -> req { message_parse_mode = Just Markdown
                      , message_text = "`/pick <item>[ <item>]*`"
                      }

instance BotModule SimpleRandom where
    bmUpdFulfiller _ = UpdFulfiller $ \case
        Update { message = Just msg }
            | Just (cmd, (_, content)) <- extractBotCommand' msg
            , cmd == "/roll" || cmd == "/pick"
            -> case cmd of
                "/roll" -> handleRoll msg content >> pure True
                "/pick" -> handlePick msg content >> pure True
                _ -> error "unreachable"
        _ -> pure False
