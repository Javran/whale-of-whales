{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  , MultiWayIf
  , ScopedTypeVariables
  , TypeApplications
  , LambdaCase
  #-}
module Javran.Wow.ProcessUpdate
  ( processUpdate
  , processKicks
  , loadState
  , saveState
  , startBot
  ) where

import Control.Monad.IO.Class
import Data.String
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Data.Time
import Data.Char
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Int
import Control.Exception

import Javran.Wow.Types
import Javran.Wow.Base
import Javran.Wow.Default ()
import Data.Default.Class
import Data.Time.Clock.POSIX
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Text.ParserCombinators.ReadP hiding (get)

int64ToT :: Int64 -> T.Text
int64ToT = T.pack . show

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

startBot :: WEnv -> Int -> IO ()
startBot wenv@WEnv{..} = fix $ \r errCount ->
    if errCount < 10
      then
        putStrLn ("Starting bot at attempt #" ++ show errCount) >>
        catch (forever run) errHandler >> r (succ errCount)
      else putStrLn "Too many errors, aborting."
  where
    run :: IO ()
    run = do
      putStrLn "Bot started"
      mgr <- newManager tlsManagerSettings
      initState <- loadState stateFile
      void $ runWowM wenv initState mgr $ do
        -- we need to do this only once at startup
        _ <- tryWithTag "DelWebhook" $ liftTC deleteWebhookM
        -- forever for update handling        
        forever $ do
          (oldSt@WPState {..}, _) <- get
          {-
            INVARIANT:
              - handleUpdate should be able to capture all ServantError inside of it.
              - same invariant for handleKicks
           -}
          mapM_ processUpdate =<< liftTC (do
            let req = def
                      { updates_offset = succ <$> lastUpdate
                      , updates_timeout = Just pullTimeout
                      }
            Response {..} <- getUpdatesM req
            pure result)
          processKicks
          (newSt, _) <- get
          when (oldSt /= newSt) saveState

    errHandler :: SomeException -> IO ()
    errHandler e =
      appendLogTo logFile $
        "Exception caught: " ++ displayException e

bumpLastSeen :: Update -> WowM ()
bumpLastSeen Update{..} = do
    -- update last seen id
    let updF oldId = Just $ if oldId < update_id then update_id else oldId
    modify (\(s@WPState{lastUpdate},rg) ->
              (s {lastUpdate = maybe (Just update_id) updF lastUpdate}, rg))

extractBotCommand :: Message -> Maybe T.Text
extractBotCommand msg
  | Message { entities = Just es, text = Just content } <- msg
  , MessageEntity {me_offset, me_length}:_ <-
      filter (\m -> me_type m == "bot_command") es
  = Just (T.toLower . T.take me_length . T.drop me_offset $ content)
  | otherwise = Nothing

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

updateToRepeatDigest :: Update -> Maybe (UTCTime, (Int, RepeatDigest))
updateToRepeatDigest upd
  | Update
    { message =
        Just msg@Message
        { date
        , from = Just User {user_id}
        }
    } <- upd
  = do
      let curTime :: UTCTime
          curTime = posixSecondsToUTCTime (fromIntegral date)
      case msg of
        Message {text = Just txt} ->
          let dg = T.filter (\x -> isPrint x && not (isSpace x)) txt
          in pure (curTime, (user_id, RepeatMessageDigest dg))
        Message {sticker = Just Sticker {sticker_file_id}} ->
          pure (curTime, (user_id, RepeatStickerDigest sticker_file_id))
        _ -> Nothing
  | otherwise = Nothing

{-
  Repeater Design:

  - remove outdated repeaterdata when start processing message
  - recognize repeated message (it make sense that we only look at current message
    and find previous messages that is the same, or almost the same)
  - do the repeating and move message into cooldown

  -}

-- should only call this function when
-- we have a sticker or text message
processRepeater :: T.Text -> Update -> WowM ()
processRepeater groupId upd
  | Just digest@(curTime, (_, rd)) <- updateToRepeatDigest upd
  = do

    window <- asks repeatWindow
    cd <- asks repeatCooldown
    let timeCut =
          -- conversion will consider UTCTime to be seconds.
          takeWhile (\(t, _) -> floor (curTime `diffUTCTime` t) <= window)
        updateRcd =
          M.filter (\t -> floor (curTime `diffUTCTime` t) <= cd)
    -- + remove outdated message digests (by cutting at time window)
    -- + remove expired cooldown
    modifyGroupState groupId $
      \gs@GroupState{repeaterDigest = rds, repeaterCooldown = rcd} ->
        gs { repeaterDigest = digest:timeCut rds
           , repeaterCooldown = updateRcd rcd
           }
    GroupState
      { repeaterDigest = newRds
      , repeaterCooldown = newCds
      } <- getGroupState groupId

    let distinctUserCount =
              IS.size
            . foldMap (\(_, (u, rd')) ->
                          if rd == rd'
                            then IS.singleton u
                            else IS.empty)
            $ newRds
    when (rd `M.member` newCds) $
      liftIO $ putStrLn "no repeat due to cooldown"
    when ((rd `M.notMember` newCds) && distinctUserCount >= 3) $ do
      -- now we should repeat
      let Update
            { message = Just msg@Message{forward_from, message_id}
            } = upd
          chatId = ChatId (read (T.unpack groupId))
      case forward_from of
        Just _ -> do
          -- forward the message
          -- for reasons we shouldn't forward from original message,
          -- (which will result in "message not found" error, flippng great design choice.)
          -- but from the message we are dealing with.
          let req = ForwardMessageRequest
                      { forward_chat_id = chatId
                      , forward_from_chat_id = chatId
                      , forward_disable_notification = Nothing
                      , forward_message_id = message_id
                      }
          _ <- tryWithTag "RepeatForward" $ liftTC (forwardMessageM req)
          pure ()
        _ ->
          case rd of
            RepeatMessageDigest {} -> do
              let Message {text = Just content} = msg
                  req = def
                          { message_chat_id = chatId
                          , message_text = content
                          }
              _ <- tryWithTag "RepeatText" $ liftTC (sendMessageM req)
              pure ()
            RepeatStickerDigest {} -> do
              let Message {sticker = Just Sticker{sticker_file_id}} = msg
                  req = SendStickerRequest
                          { sticker_chat_id = chatId
                          , sticker_sticker = sticker_file_id
                          , sticker_disable_notification = def
                          , sticker_reply_to_message_id = def
                          , sticker_reply_markup = def
                          }
              _ <- tryWithTag "RepeatSticker" $ liftTC (sendStickerM req)
              pure ()

      modifyGroupState groupId $
        \gs@GroupState{repeaterCooldown = rcd} ->
          gs {repeaterCooldown = M.insert rd curTime rcd}

  | otherwise = pure ()

userDesc :: User -> T.Text
userDesc User{..} =
    if T.length lastName > 0
      then firstName <> " " <> lastName
      else firstName
  where
    firstName = user_first_name
    lastName = fromMaybe T.empty user_last_name

yesMessages :: [[T.Text]]
yesMessages =
    [ ["说", "书", "舒"]
    , ["的", "得", "地"]
    , ["对", "队", "兑"]
    ]

noMessages :: [[T.Text]]
noMessages =
    [ ["说", "书", "舒"]
    , ["的", "得", "地"]
    , ["不", "卜", "部"]    
    , ["对", "队", "兑"]
    ]

processYorN :: Bool -> Update -> WowM ()
processYorN isYes upd
  | Update
      { message = Just msg@Message {chat = Chat {chat_id}}
      } <- upd
  , Message
      { text = Just content
      , from = Just user
      , reply_to_message
      } <- msg
    -- should trigger when it's replying to some message
  , isJust reply_to_message || T.length content > 2
  = do
      -- prioritize on replies, we only try a regular "yes or no" after
      -- pattern matching has failed
      let who = case reply_to_message of
            Just Message {from = Just u} -> userDesc u
            _ -> userDesc user
      rndMsg <- T.concat <$> mapM pickM (if isYes then yesMessages else noMessages)
      let req = def { message_chat_id = ChatId chat_id
                    , message_text = who <> rndMsg <> "!"
                    }
      _ <- tryWithTag "YorN" $ liftTC $ sendMessageM req
      pure ()
  | otherwise = pure ()

shouldProcessUpdate :: S.Set Int64 -> Update -> Bool
shouldProcessUpdate wg = \case
  Update
    { message = Just Message {chat = Chat {chat_id}}
    } ->
    chat_id `elem` wg
  Update
    { callback_query =
        Just CallbackQuery 
          { cq_message = Just Message {chat = Chat {chat_id}} }
    } ->
    chat_id `elem` wg
  _ -> False

processUpdate :: Update -> WowM ()
processUpdate upd@Update{..} = do
    bumpLastSeen upd  
    shouldProcess <- asks $
      \WEnv{watchingGroups=wg} ->
        shouldProcessUpdate wg upd
    when shouldProcess $
      case upd of
        Update
          { message =
              Just msg@Message
                { chat = Chat {chat_id}
                }
          }
          | Just cmd <- extractBotCommand msg -> do
            let sendWhale = do
                  WEnv {whaleStickers} <- ask
                  whaleSticker <- pickM whaleStickers
                  _ <- tryWithTag "Whale" $ liftTC $
                    sendStickerM ((def @(SendStickerRequest T.Text))
                                      { sticker_chat_id = ChatId chat_id
                                      , sticker_sticker = whaleSticker :: T.Text
                                      })
                  pure ()
            case cmd of
              _ | isWhaleCommand cmd -> sendWhale
              "/y" -> processYorN True  upd
              "/n" -> processYorN False upd
              _ -> liftIO $ putStrLn $ "unrecognized command: " ++ T.unpack cmd
        Update
          { callback_query =
              Just CallbackQuery
                { cq_from = User {user_id, user_username}
                , cq_message = Just Message {message_id, chat = Chat {chat_id}}
                , cq_id
                }
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
        Update
          { message =
              Just Message
                { sticker = Just Sticker {sticker_file_id}
                , chat = Chat {chat_id}
                }
          } -> do
              liftIO $ putStrLn $ "sticker received: " ++ show sticker_file_id
              liftIO $ putStrLn $ "[sticker] " ++ show upd
              processRepeater (int64ToT chat_id) upd
        Update
          { message =
            Just Message
                 { chat = Chat {chat_id = ci}
                 }
          } -> do
          liftIO $ putStrLn $ "[msg] " ++ show upd
          processRepeater (int64ToT ci) upd
        _ -> do
          let dbg = False
          when dbg $ liftIO $ putStrLn $ "-- ignored: revceived: " ++ show upd
  where
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

-- kick timed out users, and try to delete survey messages if needed
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
