module App where

import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Servant

import System.Environment

import API
import Data

import Pebble.Types


-- TODO: Probably needs a database
data Store = Store { appKeys :: [AppKey]
                   , forecasts :: [Forecast]
                   }

emptyStore :: Store
emptyStore = Store [] []

data Config = Config { coStore :: MVar Store
                     , coApiKey :: APIKey
                     }

type AppT m = ReaderT Config m

type AppM = AppT IO

stateM :: MonadIO m => State Store a -> AppT m a
stateM fn = do
        store <- asks coStore
        liftIO $ modifyMVar store $ return . fn'
    where fn' s = let (a, s') = runState fn s in (s', a)

initConfig :: IO Config
initConfig = do
    store <- newMVar emptyStore
    apiKey <- liftM APIKey $ do
        key <- getEnv "PEBBLE_API_KEY"
        when (key == "") $ error "Pebble API key must be provided."
        return key
    return $ Config store apiKey

logStr :: String -> AppM ()
logStr = liftIO . putStrLn
