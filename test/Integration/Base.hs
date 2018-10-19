module Integration.Base where

import Control.Monad.Reader

import System.Environment

import App
import Types.Config

testConfig :: IO Config
testConfig = do
  setEnv "LISTEN_PORT" "8100"
  initConfig

inApp :: Config -> AppT IO a -> IO a
inApp config = flip runReaderT config
