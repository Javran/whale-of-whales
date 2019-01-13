{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  , MultiWayIf
  , ScopedTypeVariables
  , TypeApplications
  #-}
module Javran.Wow.Worker
  ( handleUpdate
  , handleKicks
  , loadState
  , saveState
  ) where

import System.IO
import Control.Monad.IO.Class
import Data.String
import Data.Int
import Control.Monad.RWS
import Control.Monad.Catch
import Web.Telegram.API.Bot
import Data.Time
import Data.List
import System.Random
import qualified Data.UUID as UUID
import qualified Data.Text as T

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()
import Data.Default.Class

bumpLastSeen :: Update -> WowM ()
bumpLastSeen Update{..} = do
    -- update last seen id
    let updF oldId = Just $ if oldId < update_id then update_id else oldId
    modify (\s@WState{lastUpdate} -> s {lastUpdate = maybe (Just update_id) updF lastUpdate})

handleUpdate :: Update -> WowM ()
handleUpdate upd@Update{..} = do
    bumpLastSeen upd
    saveState
    case upd of
      Update
        { callback_query = Just
          CallbackQuery { cq_from = User {user_id, user_username}
                        , cq_data = Just meta
                        , cq_id
                        }
        } -> do
           WState {pendingKicks = pks} <- get
           let aReq = def { cq_callback_query_id = cq_id }
           _ <- liftTC $ answerCallbackQueryM aReq
           let (rmKicks, remainingKicks) = partition isValid pks
               isValid PendingKick{..} =
                   userId == user_id &&
                   kickMeta `T.isPrefixOf` meta
           liftIO $ putStrLn $ "kick info: " ++ show rmKicks
           liftIO $ putStrLn $ "cq info: " ++ show meta
           case rmKicks of
             [PendingKick{..}] -> do
               let cId :: Int64
                   cId = read @Int64 (T.unpack groupId)
                   welcomeMsg = case user_username of
                     Just u -> "欢迎" <> u <> "!"
                     Nothing -> "欢迎" <> fromString (show user_id) <> "!"
                   req = def { message_chat_id = ChatId cId
                             , message_text = welcomeMsg
                             } 
               _ <- liftTC $ sendMessageM req 
               pure ()
             _ -> pure ()
           modify (\s -> s {pendingKicks = remainingKicks})
      Update
        { message =
            Just Message
                 { chat = Chat {chat_type = ct, chat_id = ci} 
                , new_chat_members = Just users
                 , message_id
                 }
        }
        | ct == Group || ct == Supergroup ->
            processNewMembers message_id ci (filter (not . user_is_bot) users)
      _ -> do
        liftIO $ putStrLn $ "-- ignored: revceived: " ++ show upd
        pure ()
  where
    processNewMembers _ _ [] = pure ()
    processNewMembers msgId chatId users = do
      cbData <- UUID.toText <$> genNextM
      -- send message
      let mk txt payload = def
                 { ikb_text = txt
                 , ikb_callback_data = Just $ cbData <> "|" <> payload
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
      _ <- liftTC $ sendMessageM req
      -- adding to pending kicks
      curTime <- liftIO getCurrentTime
      let newPk :: [PendingKick]
          newPk = mkPk <$> users
          mkPk User {..} = PendingKick (fromString (show chatId)) user_id curTime cbData
      modify (\s@WState{pendingKicks = pk} -> s {pendingKicks = pk ++ newPk})

handleKicks :: WowM ()
handleKicks = do
  WEnv{..} <- ask
  WState{..} <- get
  curTime <- liftIO getCurrentTime
  let (kickingList, stillPending) = partition timeExceeded pendingKicks
      timeExceeded PendingKick{..} = floor timeDiff > kickTimeout
        where
          timeDiff = curTime `diffUTCTime` timestamp
      kickUser PendingKick{..} = tryWithTag "KickAttempt" $ liftTC $ do
        -- kick & unban, by doing so users are allowed to re-join
        _ <- kickChatMemberM groupId userId
        _ <- unbanChatMemberM groupId userId
        pure ()
        
  mapM_ kickUser kickingList
  modify (\s -> s{pendingKicks = stillPending})

loadState :: FilePath -> IO WState
loadState fp =
    catch (read <$> readFile fp) errHandler
  where
    errHandler :: SomeException -> IO WState
    errHandler e = do
      hPutStrLn stderr $ "Exception caught: " ++ displayException e
      WState Nothing [] <$> newStdGen

saveState :: WowM ()
saveState = do
    WEnv {..} <- ask
    st <- get
    liftIO $ writeFile stateFile (show st)
