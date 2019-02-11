{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  , MultiWayIf
  , ScopedTypeVariables
  , LambdaCase
  , ExistentialQuantification
  #-}
module Javran.Wow.ProcessUpdate
  ( processUpdate
  , loadState
  , saveState
  , startBot
  ) where

import Control.Monad.IO.Class
import Control.Monad.RWS
import Web.Telegram.API.Bot

import qualified Data.Set as S
import qualified Data.Text as T
import Data.Int
import Control.Exception

import Data.Default.Class
import Data.Proxy
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Javran.Wow.Base
import Javran.Wow.Default ()
import Javran.Wow.Types
import Javran.Wow.BotModule.UserVerification (UserVerification)
import Javran.Wow.BotModule.SendWhale (SendWhale)
import Javran.Wow.BotModule.SendYesOrNo (SendYesOrNo)
import Javran.Wow.BotModule.Lmgtfy (Lmgtfy)
import Javran.Wow.BotModule.Chiba (Chiba)
import Javran.Wow.BotModule.SimpleRandom (SimpleRandom)
import Javran.Wow.BotModule.CommandSink (CommandSink)
import Javran.Wow.BotModule.Repeater (Repeater)

data BMod = forall bm. BotModule bm => BMod (Proxy bm)

-- list of modules
-- note that both updates and post-processings are handled in the same order
botMods :: [BMod]
botMods =
    [ BMod (Proxy :: Proxy UserVerification)
    , BMod (Proxy :: Proxy SendWhale)
    , BMod (Proxy :: Proxy SendYesOrNo)
    , BMod (Proxy :: Proxy Lmgtfy)
    , BMod (Proxy :: Proxy Chiba)
    , BMod (Proxy :: Proxy SimpleRandom)
      -- NOTE: CommandSink consumes all commands,
      -- no bot command modules should be placed after it.
    , BMod (Proxy :: Proxy CommandSink)
    , BMod (Proxy :: Proxy Repeater)
    ]

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
        {-
          print info about itself at startup.
          by doing so we confirm the API is working
          and inform developer about bot's user id if it is not yet known
         -}
        eMeInfo <- tryWithTag "GetMe" $ liftTC getMeM
        case eMeInfo of
          Nothing -> pure ()
          Just Response{result=User{..}} ->
            liftIO $ do
              let trM = maybe "<Nothing>" T.unpack
              putStrLn $ "User Id: " <> show user_id
              putStrLn $ "First Name: " <> T.unpack user_first_name
              putStrLn $ "Last Name: " <> trM user_last_name
              putStrLn $ "Username: " <> trM user_username
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
          mapM_ (\(BMod bm) -> bmPostProcess bm) botMods
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
    when shouldProcess $ do
      let combinedUpdateProcessor =
            getUpdFulfiller (foldMap (\(BMod m) -> bmUpdFulfiller m) botMods)
      -- process updates using modules
      processed <- combinedUpdateProcessor upd
      unless processed $
        case upd of
          Update
            { message = Just Message { sticker = Just Sticker {sticker_file_id}}
            } -> do
                  liftIO $ putStrLn $ "sticker received: " ++ show sticker_file_id
                  liftIO $ putStrLn $ "[sticker] " ++ show upd
          Update
            { message = Just Message{}} ->
            liftIO $ putStrLn $ "[msg] " ++ show upd
          _ -> do
            let dbg = False
            when dbg $ liftIO $ putStrLn $ "-- ignored: revceived: " ++ show upd
