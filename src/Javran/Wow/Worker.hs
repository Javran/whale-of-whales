{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  , MultiWayIf
  , ScopedTypeVariables
  , TupleSections
  #-}
module Javran.Wow.Worker
  ( handleUpdate
  , handleKicks
  , loadState
  , saveState
  ) where

import Control.Monad.IO.Class
import Data.String
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Data.Time
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Data.Set as S

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()
import Data.Default.Class

bumpLastSeen :: Update -> WowM ()
bumpLastSeen Update{..} = do
    -- update last seen id
    let updF oldId = Just $ if oldId < update_id then update_id else oldId
    modify (\(s@WPState{lastUpdate},rg) ->
              (s {lastUpdate = maybe (Just update_id) updF lastUpdate}, rg))

handleUpdate :: Update -> WowM ()
handleUpdate upd@Update{..} = do
    bumpLastSeen upd
    case upd of
      Update
        { callback_query =
            Just CallbackQuery
              { cq_from = User {user_id, user_username}
              , cq_message = Just Message {message_id, chat = Chat {chat_id}}
              , cq_id
              }
        } -> do
           let curChatId = T.pack (show chat_id)
           (WPState {pendingKicks = pks},_) <- get
           case M.lookup (message_id, curChatId) pks of
             Nothing -> pure ()
             Just UserVerificationMessage {..} -> do
               let aReq = def { cq_callback_query_id = cq_id }
               _ <- tryWithTag "VerifAns" $ liftTC $ answerCallbackQueryM aReq
               when (user_id `IS.member` userSet) $ do
                 -- user has completed verification
                 -- remove record and say hi
                 let modifyUVM x@UserVerificationMessage{userSet = us} =
                       x {userSet = IS.delete user_id us}
                     modifyKick = M.adjust modifyUVM (message_id, curChatId)
                 modify (\(s,rg) ->
                          (s {pendingKicks = modifyKick pks}, rg))
                 let welcomeMsg = case user_username of
                       Just u -> "欢迎" <> u <> "!"
                       Nothing -> "欢迎" <> fromString (show user_id) <> "!"
                     req = def { message_chat_id = ChatId chat_id
                               , message_text = welcomeMsg
                               }
                 _ <- tryWithTag "VerifSayHi" $ liftTC $ sendMessageM req
                 pure ()
      Update
        { message =
            Just Message
                 { chat = Chat {chat_type = ct, chat_id = ci}
                , new_chat_members = Just users
                 , message_id
                 }
        }
        | ct == Group || ct == Supergroup ->
            processNewMembers message_id ci users
      _ -> do
        let dbg = False
        when dbg $ liftIO $ putStrLn $ "-- ignored: revceived: " ++ show upd
  where
    processNewMembers msgId chatId usersInp = do
        (WPState {pendingKicks}, _) <- get
        let curGroupId = T.pack (show chatId)
            -- extract pending users (for this group)
            pendingUsers :: IS.IntSet
            pendingUsers = M.foldrWithKey extractUser IS.empty pendingKicks
              where
                extractUser (_, groupId) uvm s1 =
                  if curGroupId == groupId
                    then userSet uvm `IS.union` s1
                    else s1
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
            let modifyKick =
                  M.insert
                    (respMsgId, curGroupId)
                    UserVerificationMessage
                      { timestamp
                      , userSet = IS.fromList (user_id <$> users)
                      }
            modify (\(s@WPState {pendingKicks = pks},rg) ->
                      (s {pendingKicks = modifyKick pks}, rg))

-- kick timed out users, and try to delete survey messages if needed
handleKicks :: WowM ()
handleKicks = do
  WEnv{kickTimeout} <- ask
  (WPState{pendingKicks},_) <- get
  curTime <- liftIO getCurrentTime
  let timeExceeded UserVerificationMessage{timestamp} =
          floor timeDiff > kickTimeout
        where
          timeDiff = curTime `diffUTCTime` timestamp
      (outdatedKicks, stillPendingKicks) = M.partition timeExceeded pendingKicks
      spToClear0 :: S.Set (Int, T.Text)
      spToClear0 = M.keysSet outdatedKicks
      -- collect users that we want to kick out
      kickingUsers =
        M.foldrWithKey
          (\k UserVerificationMessage{userSet} xs ->
             ((k,) <$> IS.toList userSet) ++ xs)
          []
          outdatedKicks
  forM_ kickingUsers $ \((_, groupId), userId) ->
    tryWithTag "KickAttempt" $
      liftTC $ do
        -- kick & unban, by doing so users are allowed to re-join
        _ <- kickChatMemberM groupId userId
        _ <- unbanChatMemberM groupId userId
        pure ()

  -- verif messge comes from two places:
  -- oudated messages(0) + those that no longer contain users(1)
  let (spToClearPre1, pendingKicks') = M.partition isEmpty stillPendingKicks
        where
          isEmpty u = IS.null (userSet u)
      spToClear :: S.Set (Int, T.Text)
      spToClear = spToClear0 `S.union` M.keysSet spToClearPre1
  modify (\(s,rg) ->
             (s {pendingKicks = pendingKicks'}, rg))
  -- remove verif messages
  _ <- tryWithTag "VerifCleanup" $ liftTC $
    forM (S.toList spToClear) $ \(msgId, groupId) -> do
      let req = def
                { del_msg_chat_id = ChatId (read (T.unpack groupId))
                , del_msg_message_id = msgId
                }
      deleteMessageM req
  pure ()
