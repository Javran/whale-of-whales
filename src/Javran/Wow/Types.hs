{-# LANGUAGE
    ScopedTypeVariables
  , ExplicitForAll
  , NamedFieldPuns
  , StandaloneDeriving
  , DeriveGeneric
  , RecordWildCards
  , OverloadedStrings
  , LambdaCase
  , MultiWayIf
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Javran.Wow.Types
  ( UserVerificationMessage(..)
  , WState
  , GroupState(..)
  , WPState(..)
  , WEnv(..)
  , RepeatDigest(..)
  , WowM
  ) where

import Data.Int
import Data.Time
import Control.Monad.RWS
import Web.Telegram.API.Bot
import Servant.Client
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import System.Random
import Data.Default.Class
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

deriving instance Eq ChatType

instance Default (IM.IntMap a) where
  def = mempty

data UserVerificationMessage = UserVerificationMessage
  { timestamp :: UTCTime
  , userSet :: IS.IntSet
  } deriving (Read, Show, Eq)

instance ToJSON UserVerificationMessage where
  toJSON UserVerificationMessage {..} =
    object [ "timestamp" .= timestamp
           , "user-set" .= userSet
           ] 

instance FromJSON UserVerificationMessage where
  parseJSON (Object o) =
    UserVerificationMessage
      <$> o .: "timestamp"
      <*> o .: "user-set"
  parseJSON invalid = typeMismatch "UserVerificationMessage" invalid

data GroupState = GroupState
  { pendingKicks :: IM.IntMap UserVerificationMessage
    -- element is: (<time>, (<user id>, <RepeatDigest>))
    -- INVARIANT:
    -- - must be sorted in descending order of UTCTime
  , repeaterDigest :: [(UTCTime, (Int, RepeatDigest))]
  , repeaterCooldown :: M.Map RepeatDigest UTCTime
  } deriving (Read, Show, Eq, Generic)

instance Default GroupState 

instance FromJSON GroupState where
  parseJSON (Object o) =
    GroupState
      <$> o .: "pending-kicks"
      <*> o .: "repeater-digest"
      <*> o .: "repeater-cooldown"
  parseJSON invalid = typeMismatch "GroupState" invalid      

instance ToJSON GroupState where
  toJSON GroupState {..} =
    object [ "pending-kicks" .= pendingKicks
           , "repeater-digest" .= repeaterDigest
           , "repeater-cooldown" .= repeaterCooldown
           ]
    
-- "P" for persistent
data WPState = WPState
  { lastUpdate :: Maybe Int
  , groupStates :: M.Map T.Text GroupState
  } deriving (Read, Show, Eq, Generic)

instance ToJSON WPState where
  toJSON WPState{..} =
    object [ "last-update" .= lastUpdate
           , "group-states" .= groupStates
           ]

instance FromJSON WPState where
  parseJSON (Object o) =
    WPState
      <$> o .:? "last-update"
      <*> o .: "group-states"
  parseJSON invalid = typeMismatch "WPState" invalid

instance Default (M.Map a b) where
  def = M.empty

instance Default WPState

-- separation of persistent state vs. runtime-only ones
type WState = (WPState, StdGen)

data WEnv = WEnv
  { botToken :: Token
  , pullTimeout :: Int
  , kickTimeout :: Int
  , logFile :: FilePath
  , stateFile :: FilePath
  , watchingGroups :: S.Set Int64
  , whaleStickers :: [T.Text]
  , repeatCooldown :: Int
  , repeatWindow :: Int
  } deriving (Generic)

instance ToJSON WEnv where
  toJSON WEnv{..} =
      object [ "bot-token" .= botTokStr
             , "pull-timeout" .= pullTimeout
             , "kick-timeout" .= kickTimeout
             , "log-file" .= logFile
             , "state-file" .= stateFile
             , "watching-groups" .= watchingGroups
             , "whale-stickers" .= whaleStickers
             , "repeat-cooldown" .= repeatCooldown
             , "repeat-window" .= repeatWindow
             ]
    where
      Token botTokStr = botToken

instance FromJSON WEnv where
  parseJSON (Object o) =
      WEnv
        <$> (Token <$> (o .: "bot-token"))
        <*> o .: "pull-timeout"
        <*> o .: "kick-timeout"
        <*> o .: "log-file"
        <*> o .: "state-file"
        <*> o .: "watching-groups"
        <*> o .: "whale-stickers"
        <*> o .: "repeat-cooldown"
        <*> o .: "repeat-window"
  parseJSON invalid = typeMismatch "WEnv" invalid

{-
  mechanism for repeater cooldown

  the idea is that bot shouldn't repeat message too much.
  we restrict bot from repeating a message by putting it into a cooldown.
  meanwhile, bot are allowed to repeat whatever she want but whatever is sent
  shouldn't match anything from cooldown and the sent message is put into
  cooldown.

  note that RepeatDigest does not exactly represent original message:

  - for RepeatMesssageDigest, spaces and non-printables are removed
  - for RepeatStickerDigest, file id must be valid

  by design, repeating message is recognized by
  looking at last update and check against the sequence of RepeatDigest,
  which means the very message that we are recognizing should be
  the single source of truth:

  - if it's a forwarding message, we do the same
  - if it's not, we send message instead of forwarding,
    and content should be copied exactly from the message we are recognizing
    instead of using data from Digest

 -}
data RepeatDigest
  = RepeatMessageDigest T.Text -- ^ sanitized message
  | RepeatStickerDigest T.Text -- ^ sticker, payload should be a valid file id
    deriving (Read, Show, Eq, Ord)

instance ToJSON RepeatDigest where
  toJSON = \case
    RepeatMessageDigest x -> String ("msg:" <> x)
    RepeatStickerDigest x -> String ("stk:" <> x)

instance ToJSONKey RepeatDigest

instance FromJSON RepeatDigest where
  parseJSON = withText "RepeatDigest" $ \x ->
    if | "msg:" `T.isPrefixOf` x -> pure (RepeatMessageDigest $ T.drop 4 x)
       | "stk:" `T.isPrefixOf` x -> pure (RepeatStickerDigest $ T.drop 4 x)
       | otherwise -> fail "unrecognized text"

instance FromJSONKey RepeatDigest

type WowM a = RWST WEnv () WState ClientM a
