{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  #-}
module Javran.Wow.BotModule.UserVerification
  (
  ) where

import Javran.Wow.Types
import Web.Telegram.API.Bot
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Control.Monad.RWS
import Data.Time.Clock.POSIX

import Data.Default.Class
import Data.Maybe

import Javran.Wow.Default ()
import Javran.Wow.Base

data UserVerification

getGroupState :: T.Text -> WowM GroupState
getGroupState chatId =
  gets $ fromMaybe def . M.lookup chatId . groupStates . fst

modifyGroupState :: T.Text -> (GroupState -> GroupState) -> WowM ()
modifyGroupState chatId f =
    modify (\(s@WPState{groupStates = gss},rg) ->
              (s {groupStates = M.alter f' chatId gss}, rg))
  where
    f' Nothing = Just (f def)
    f' (Just x) = Just (f x)

processNewMembers msgId chatId usersInp = do
        let curGroupId = T.pack (show chatId)
        GroupState {pendingKicks} <- getGroupState curGroupId
            -- extract pending users (for this group)
        let pendingUsers :: IS.IntSet
            pendingUsers = foldMap userSet pendingKicks
            shouldVerify user =
              not (user_is_bot user) && not (user_id user `IS.member` pendingUsers)
        -- we should only verify non-bot users,
        -- also if a user is in the process of verification,
        -- we need to make sure re-entering does nothing.
        case filter shouldVerify usersInp of
          -- from this point on, we shouldn't use "usersInp" any more.
          [] -> pure ()
          users -> do
            -- send message
            let mk txt payload = def
                                 { ikb_text = txt
                                 , ikb_callback_data = Just payload
                                 }
                req = def
                      { message_chat_id = ChatId chatId
                      , message_text = "是萌妹子吗?"
                      , message_reply_to_message_id = Just msgId
                      , message_reply_markup = Just (ReplyInlineKeyboardMarkup
                                               [ [mk "是" "y"]
                                               , [mk "不是" "n"]
                                               , [mk "114514" "s"]
                                               ])
                      }
            Response{result = Message {message_id = respMsgId}} <- liftTC $ sendMessageM req
            timestamp <- liftIO getCurrentTime
            let modifyKick gs@GroupState{pendingKicks = pks} =
                    gs {pendingKicks = IM.insert respMsgId uvm pks}
                  where
                    uvm = UserVerificationMessage
                          { timestamp
                          , userSet = IS.fromList (user_id <$> users)
                          }
            modifyGroupState curGroupId modifyKick


instance BotModule UserVerification where
  bmUpdFulfiller _ = UpdFulfiller $ \case
    Update
      { message = Just Message
          { chat = Chat {chat_type = ct, chat_id = ci}
          , new_chat_members = Just users
          , message_id
          }
      }
      | ct == Group || ct == Supergroup -> do
        processNewMembers message_id ci users
        pure True
