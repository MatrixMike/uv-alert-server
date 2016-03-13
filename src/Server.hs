{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
module Server where

import Control.Lens

import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Data.List
import qualified Data.Set as S

import Network.Wai

import Servant

import API
import App
import Types
import Types.Location


type AppSM = AppT (EitherT ServantErr IO)

server :: ServerT API AppSM
server = registerApp :<|> getForecast :<|> getLocations

readerToEither :: Config -> AppSM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve api (readerServer cfg)

registerApp :: AppKey -> AppSM ()
registerApp key = stateM $ stAppKeys %= (++ [key])

getForecast :: Location -> AppSM [Forecast]
getForecast loc = do
    forecasts <- stateM $ use stForecasts
    let locForecasts = sortBy compareUpdated $ S.toList $ S.filter ((== loc) . view fcLocation) forecasts
    case locForecasts of
        [] -> lift $ left $ err404 { errBody = "Location not found" }
        _ -> return locForecasts

getLocations :: AppSM [Location]
getLocations = do
    forecasts <- stateM $ use stForecasts
    -- FIXME: how to map a Lens over a Set?
    return $ S.toList $ S.map (view fcLocation) forecasts
