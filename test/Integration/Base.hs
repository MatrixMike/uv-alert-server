module Integration.Base where

import Control.Monad.Reader

import System.Environment

import App

testConfig :: IO Config
testConfig = do
    setEnv "PEBBLE_API_KEY" "test"
    setEnv "LISTEN_PORT" "8100"
    initConfig

inApp :: Config -> AppT IO a -> IO a
inApp config = flip runReaderT config
