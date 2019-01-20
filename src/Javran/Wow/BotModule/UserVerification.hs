{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  #-}
module Javran.Wow.BotModule.UserVerification
  ( UserVerification
  ) where

import Data.Int (Int64)
import Control.Monad.RWS
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time (diffUTCTime)
import Data.Default.Class
import Web.Telegram.API.Bot

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Util
import Javran.Wow.Default ()

data UserVerification

-- for every new chat member joining, we send out a message to initiate verification
processNewMembers :: Int -> Int64 -> [User] -> WowM ()
processNewMembers msgId chatId usersInp = do
    let curGroupId = T.pack (show chatId)
    GroupState {pendingKicks} <- getGroupState curGroupId
    -- extract pending users (for this group)
    let pendingUsers :: IS.IntSet
        pendingUsers = foldMap userSet pendingKicks
        -- we should only verify non-bot users,
        -- also we should ignore users already entered the pending kicks list
        shouldVerify user =
            not (user_is_bot user) && not (user_id user `IS.member` pendingUsers)
    case filter shouldVerify usersInp of
        -- from this point on, we shouldn't use "usersInp" any more.
        [] -> pure ()
        users -> do
            -- send message
            let mk txt payload =
                  def { ikb_text = txt
                      , ikb_callback_data = Just payload
                      }
                req = def
                      { message_chat_id = ChatId chatId
                      , message_text = "是萌妹子吗?"
                      , message_reply_to_message_id = Just msgId
                      , message_reply_markup =
                          Just (ReplyInlineKeyboardMarkup
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
                    uvm =
                      UserVerificationMessage
                        { timestamp
                        , userSet = IS.fromList (user_id <$> users)
                        }
            modifyGroupState curGroupId modifyKick

processUserAction :: CallbackQuery -> WowM ()
processUserAction = \case
    CallbackQuery
        { cq_from = user@User {user_id}
        , cq_message = Just Message {message_id, chat = Chat {chat_id}}
        , cq_id
        } -> do
        let curChatId = T.pack (show chat_id)
        groupState <- getGroupState curChatId
        let GroupState {pendingKicks = pks} = groupState
        case IM.lookup message_id pks of
            Nothing -> pure ()
            Just UserVerificationMessage {..} -> do
                 let aReq = def { cq_callback_query_id = cq_id }
                 _ <- tryWithTag "VerifAns" $ liftTC $ answerCallbackQueryM aReq
                 when (user_id `IS.member` userSet) $ do
                     -- user has completed verification
                     -- remove record and say hi
                     let modifyUVM x@UserVerificationMessage{userSet = us} =
                           x {userSet = IS.delete user_id us}
                         modifyKick :: GroupState -> GroupState
                         modifyKick gs@GroupState {pendingKicks = pks'} =
                           gs {pendingKicks = IM.adjust modifyUVM message_id pks'}
                     modifyGroupState curChatId modifyKick
                     let who = userDesc user
                         req = def { message_chat_id = ChatId chat_id
                                   , message_text = "欢迎 " <> who <> " !"
                                   }
                     _ <- tryWithTag "VerifSayHi" $ liftTC $ sendMessageM req
                     pure ()
    _ -> pure ()

-- kick timed out users, and try to delete the initial message if needed
processKicks :: WowM ()
processKicks = do
    WEnv{kickTimeout} <- ask
    curTime <- liftIO getCurrentTime
    let timeExceeded UserVerificationMessage{timestamp} =
            floor timeDiff > kickTimeout
          where
            timeDiff = curTime `diffUTCTime` timestamp
    (WPState{groupStates = gss}, _) <- get
    let processKicksByGroup :: T.Text -> GroupState -> WowM GroupState
        processKicksByGroup groupId gs@GroupState{pendingKicks} = do
          let (outdatedKicks, stillPendingKicks) = IM.partition timeExceeded pendingKicks
              -- collect verif message ids
              spToClear0 :: IS.IntSet
              spToClear0 = IM.keysSet outdatedKicks
              -- collect users that we want to kick out
              kickingUsers = IS.toList $ foldMap userSet outdatedKicks
          forM_ kickingUsers $ \userId ->
            tryWithTag "KickAttempt" $
              liftTC $ do
                -- kick & unban, by doing so users are allowed to re-join
                _ <- kickChatMemberM groupId userId
                _ <- unbanChatMemberM groupId userId
                pure ()
          -- verif messge comes from two places:
          -- oudated messages(0) + those that no longer contain users(1)
          let (spToClearPre1, pendingKicks') = IM.partition isEmpty stillPendingKicks
                where
                  isEmpty u = IS.null (userSet u)
              spToClear :: IS.IntSet
              spToClear = spToClear0 `IS.union` IM.keysSet spToClearPre1
          -- remove verif messages
          _ <- tryWithTag "VerifCleanup" $ liftTC $
            forM (IS.toList spToClear) $ \msgId -> do
                let req = def
                          { del_msg_chat_id = ChatId (read (T.unpack groupId))
                          , del_msg_message_id = msgId
                          }
                deleteMessageM req
          pure gs{pendingKicks = pendingKicks'}

    gss' <- M.traverseWithKey processKicksByGroup gss
    modify (\(s,gs) -> (s{groupStates = gss'}, gs))

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
              -- step #1: initiate message
              processNewMembers message_id ci users
              pure True
        Update { callback_query = Just cq} -> do
              -- step #2: user action
              -- user should click on any of those buttons
              -- so that we can discharge from kicks
              processUserAction cq
              pure True
        _ ->
              pure False

    bmPostProcess _ = processKicks
