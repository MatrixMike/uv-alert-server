{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
module Server where

import Control.Concurrent.MVar

import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Network.Wai

import Servant

import API
import App
import Data


type AppSM = AppT (EitherT ServantErr IO)

server :: ServerT API AppSM
server = registerApp :<|> getForecast

readerToEither :: Config -> AppSM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve api (readerServer cfg)

registerApp :: AppKey -> AppSM ()
registerApp key = do
    stateM $ modify $
        \store -> store { appKeys = appKeys store ++ [key] }

getForecast :: Location -> AppSM Forecast
getForecast loc = do
    forecasts <- stateM $ gets forecasts
    let locForecasts = filter ((== loc) . location) forecasts
    case locForecasts of
        [] -> lift $ left $ err404 { errBody = "Location not found" }
        fc:_ -> return fc
