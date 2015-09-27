module Server where

import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Servant

import API
import Data


data Config = Config

type AppM = ReaderT Config (EitherT ServantErr IO)

server :: ServerT API AppM
server = registerApp :<|> getForecast

registerApp :: APIKey -> AppM ()
registerApp = undefined

getForecast :: Location -> AppM Forecast
getForecast = undefined
