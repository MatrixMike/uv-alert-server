{-# Language TypeOperators #-}
module App where

import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Network.Wai

import Servant

import API
import Server


readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: IO (Server API)
readerServer = do
    store <- newMVar emptyStore
    let cfg = Config store
    return $ enter (readerToEither cfg) server

app :: IO Application
app = liftM (serve api) readerServer
