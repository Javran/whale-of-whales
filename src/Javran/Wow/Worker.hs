{-# LANGUAGE RecordWildCards, NamedFieldPuns, StandaloneDeriving, OverloadedStrings, MultiWayIf #-}
module Javran.Wow.Worker
  ( handleUpdate
  , handleKicks
  , loadState
  , saveState
  ) where

import System.IO
import Control.Monad.IO.Class
import Data.String
import Control.Exception
import Web.Telegram.API.Bot
import Data.Time
import Data.Time.Clock
import Data.List

import Javran.Wow.Types

-- TODO: move to Types when done
deriving instance Eq ChatType

bumpLastSeen :: Update -> WowM ()
bumpLastSeen Update{..} = do
    -- update last seen id
    let updF oldId = Just $ if oldId < update_id then update_id else oldId
    modifyWState (\s@WState{lastUpdate} -> s {lastUpdate = maybe (Just update_id) updF lastUpdate})

handleUpdate :: Update -> WowM ()
handleUpdate upd@Update{..} = do
    bumpLastSeen upd
    case upd of
      Update
        { callback_query = Just cq@CallbackQuery {..}
        } ->
           -- TODO: cq handling
           liftIO $ putStrLn $ "CQ: " ++ show cq
      Update
        { message =
            Just Message
                 { chat = Chat {chat_type = ct, chat_id = ci}
                 , new_chat_members = Just users@(_:_)
                 , message_id
                 }
        }
        | ct == Group || ct == Supergroup ->
            processNewMembers message_id ci (filter (not . user_is_bot) users)
      _ -> do
        liftIO $ putStrLn $ "-- ignored: revceived: " ++ show upd
        pure ()
  where
    processNewMembers msgId chatId users = do
      -- send message
      let mk :: Int -> InlineKeyboardButton
          mk n = InlineKeyboardButton
                 { ikb_text = "Option #" <> fromString (show n)
                 , ikb_url = Nothing
                 , ikb_callback_data = Just $ "option_" <> fromString (show n)
                 , ikb_switch_inline_query = Nothing
                 , ikb_callback_game = Nothing
                 , ikb_switch_inline_query_current_chat = Nothing
                 , ikb_pay = Nothing
                 } 
          req = SendMessageRequest
                { message_chat_id = ChatId chatId
                , message_text = "new member!"
                , message_parse_mode = Nothing
                , message_disable_web_page_preview = Nothing
                , message_disable_notification = Nothing
                , message_reply_to_message_id = Just msgId
                , message_reply_markup = Just (ReplyInlineKeyboardMarkup
                                               [ [mk 1]
                                               , [mk 2]
                                               , [mk 3]
                                               , [mk 4]
                                               ])
                }
      _ <- liftTC $ sendMessageM req
      -- adding to pending kicks
      curTime <- liftIO getCurrentTime
      let newPk :: [PendingKick]
          newPk = mkPk <$> users
          mkPk User {..} = PendingKick (fromString (show chatId)) user_id curTime
      modifyWState (\s@WState{pendingKicks = pk} -> s {pendingKicks = pk ++ newPk})

handleKicks :: WowM ()
handleKicks = do
  WEnv{..} <- asksWEnv id
  WState{..} <- getWState
  curTime <- liftIO getCurrentTime
  let (kickingList, stillPending) = partition timeExceeded pendingKicks
      timeExceeded PendingKick{..} = floor timeDiff > kickTimeout
        where
          timeDiff = curTime `diffUTCTime` timestamp
      kickUser PendingKick{..} = liftTC $ do
        -- kick & unban, by doing so users are allowed to re-join
        _ <- kickChatMemberM channelId userId
        _ <- unbanChatMemberM channelId userId
        pure ()
        
  mapM_ kickUser kickingList
  modifyWState (\s -> s{pendingKicks = stillPending})

loadState :: FilePath -> IO WState
loadState fp =
    catch (read <$> readFile fp) errHandler
  where
    errHandler :: SomeException -> IO WState
    errHandler e = do
      hPutStrLn stderr $ "Exception caught: " ++ displayException e
      pure (WState Nothing [])

saveState :: WowM ()
saveState = do
    WEnv {..} <- asksWEnv id
    st <- getWState
    liftIO $ writeFile stateFile (show st)
