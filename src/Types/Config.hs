{-# LANGUAGE TemplateHaskell #-}

module Types.Config where

import Control.Concurrent.MVar

import Control.Lens

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Reader

import qualified Data.Set as S

import Types
import Types.Location

data Fetcher = Fetcher
  { fName :: String
  , fFetch :: AppM [Forecast]
  , fLocations :: [LocationCoordinates]
  }

data Config = Config
  { coStore :: MVar Store
  , coListenPort :: Int
  , coFetchers :: [Fetcher]
  }

type AppT m = ReaderT Config m

type AppM = AppT IO

data Store = Store
  { _stAppKeys :: [AppKey]
  , _stForecasts :: S.Set Forecast
  }

makeLenses ''Store

emptyStore :: Store
emptyStore = Store [] S.empty

stateM :: MonadIO m => State Store a -> AppT m a
stateM fn = do
  store <- asks coStore
  liftIO $ modifyMVar store $ return . fn'
  where
    fn' s =
      let (a, s') = runState fn s
      in (s', a)

logStr :: String -> AppM ()
logStr = liftIO . putStrLn
