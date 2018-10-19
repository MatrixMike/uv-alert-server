{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Lens

import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.List
import qualified Data.Set as S

import Network.Wai

import Servant

import API
import Types
import Types.Config
import Types.Location

type AppSM = AppT Handler

server :: ServerT API AppSM
server = registerApp :<|> getForecast :<|> getLocations

readerToExcept :: Config -> AppSM a -> Handler a
readerToExcept cfg x = runReaderT x cfg

readerServer :: Config -> Server API
readerServer cfg = hoistServer api (readerToExcept cfg) server

app :: Config -> Application
app cfg = serve api (readerServer cfg)

registerApp :: AppKey -> AppSM ()
registerApp key = stateM $ stAppKeys %= (++ [key])

allLocations :: AppSM [LocationCoordinates]
allLocations = asks $ concatMap fLocations . coFetchers

getForecast :: LocationT coord tz -> AppSM [Forecast]
getForecast loc = do
  let loc' = toBaseLocation loc
  locations <- allLocations
  when (loc' `notElem` ((withoutTZ . withoutCoordinates) <$> locations)) $
    lift $ throwError $ err404 {errBody = "Location not found"}
  forecasts <- stateM $ use stForecasts
  let baseLocationEq :: LocationT coord tz -> LocationT coord' tz' -> Bool
      baseLocationEq l1 l2 = toBaseLocation l1 == toBaseLocation l2
  return $
    sortBy compareUpdated $
    S.toList $ S.filter (baseLocationEq loc . view fcLocation) forecasts

getLocations :: AppSM [LocationCoordinates]
getLocations = sort <$> allLocations
