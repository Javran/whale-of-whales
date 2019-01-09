module Javran.Wow.Worker
  ( handleUpdate
  , handleKick
  , loadState
  , saveState
  ) where

import Web.Telegram.API.Bot

import Javran.Wow.Types

handleUpdate :: Update -> WowM ()
handleUpdate = undefined

handleKick :: WowM ()
handleKick = undefined

loadState :: IO WState
loadState = undefined

saveState :: WowM ()
saveState = undefined
