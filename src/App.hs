module App where

import Control.Concurrent.MVar

import Data.Maybe

import System.Environment

import Fetcher.Australia.Arpansa
import Fetcher.EPA
import Fetcher.JMA
import Types.Config

defaultFetchers :: [Fetcher]
defaultFetchers = [arpansaFetcher, epaFetcher, jmaFetcher]

initConfig :: IO Config
initConfig = do
  store <- newMVar emptyStore
  listenPort <- read . fromMaybe "8000" <$> lookupEnv "LISTEN_PORT"
  return
    Config
    { coStore = store
    , coListenPort = listenPort
    , coFetchers = defaultFetchers
    }
