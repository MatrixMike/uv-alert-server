module App where

import Control.Concurrent.MVar

import Control.Monad

import Data.Maybe

import System.Environment

import Fetcher.Arpansa
import Fetcher.EPA
import Fetcher.JMA
import Types.Config

import Pebble.Types

defaultFetchers :: [Fetcher]
defaultFetchers = [arpansaFetcher, epaFetcher, jmaFetcher]

initConfig :: IO Config
initConfig = do
  store <- newMVar emptyStore
  apiKey <-
    liftM APIKey $ do
      key <- getEnv "PEBBLE_API_KEY"
      when (key == "") $ error "Pebble API key must be provided."
      return key
  listenPort <- liftM (read . fromMaybe "8000") $ lookupEnv "LISTEN_PORT"
  return $
    Config
    { coStore = store
    , coApiKey = apiKey
    , coListenPort = listenPort
    , coFetchers = defaultFetchers
    }
