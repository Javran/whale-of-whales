{-# LANGUAGE
    OverloadedStrings
  #-}
module Javran.WoW.Main
  ( main
  ) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot
import System.Environment
import Data.String

getToken :: IO Token
getToken = Token . fromString <$> getEnv "BOT_TOKEN"

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  tok <- getToken
  result <- runTelegramClient tok mgr $ do
    info <- getWebhookInfoM
    isSet <- setWebhookM (setWebhookRequest' "https://example.com/hook")
    getMeM
  print result
  putStrLn "done"
