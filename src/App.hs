{-# Language TypeOperators #-}
module App where

import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Network.Wai

import Servant

import API
import Server


readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve api (readerServer cfg)
