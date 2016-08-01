module App where

import Control.Concurrent.MVar

import Control.Lens

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Maybe

import System.Environment

import Fetcher
import Fetcher.Arpansa
import Fetcher.EPA
import Types
import Types.Config

import Pebble.Types


defaultFetchers :: [Fetcher]
defaultFetchers = [arpansaFetcher, epaFetcher]

initConfig :: IO Config
initConfig = do
    store <- newMVar emptyStore
    apiKey <- liftM APIKey $ do
        key <- getEnv "PEBBLE_API_KEY"
        when (key == "") $ error "Pebble API key must be provided."
        return key
    listenPort <- liftM (read . fromMaybe "8000") $ lookupEnv "LISTEN_PORT"
    return $ Config store apiKey listenPort defaultFetchers
