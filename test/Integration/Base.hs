module Integration.Base where

import Control.Monad.Reader

import Data.Aeson

import System.Environment

import App
import Server

testConfig :: IO Config
testConfig = do
    setEnv "PEBBLE_API_KEY" "test"
    setEnv "LISTEN_PORT" "8100"
    initConfig

inApp :: Config -> AppT IO a -> IO a
inApp config = flip runReaderT config
