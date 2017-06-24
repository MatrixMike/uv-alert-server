{-# LANGUAGE OverloadedStrings #-}

module Fetcher.EPA where

{- Fetch USA data from EPA API. -}
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Network.HTTP.Client
import Network.HTTP.Simple

import Fetcher.Base
import Fetcher.EPA.Cities
import Types
import Types.Config
import Types.Location

epaFetcher :: Fetcher
epaFetcher = Fetcher "EPA" fetchEpa usLocations

fetchEpa :: AppM [Forecast]
fetchEpa = do
  liftM concat $ forM usLocations $ \location -> do
    logStr $ "Fetching forecast for " ++ show location ++ "..."
    let address = forecastAddress location
    logErrors address $ do
      response <- parseRequest address >>= httpJSON
      time <- liftIO getCurrentTime
      let measurements =
            map
              (\fi -> (fiDateTime location fi, fiLevel fi))
              (responseBody response)
      return $ maybeToList $ buildForecast location time measurements

forecastAddress :: Location -> String
forecastAddress location =
  "https://iaspub.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/CITY/" ++ city ++
  "/STATE/" ++
  abbr ++
  "/JSON"
  where
    city = location ^. locCity
    abbr = location ^. locRegion . to usStateAbbreviation

data ForecastItem = ForecastItem
  { fiLocalTime :: LocalTime
  , fiLevel :: UVLevel
  }

instance FromJSON ForecastItem where
  parseJSON =
    withObject "forecast item" $ \v -> do
      level <- liftM UVLevel $ v .: "UV_VALUE"
      time <- (v .: "DATE_TIME") >>= parseLocalTime
      return $ ForecastItem time level
    where
      parseLocalTime = parseTimeM False defaultTimeLocale "%b/%d/%Y %I %P"

-- Parse a date from the forecast in a format: MAR/17/2016 11 PM
fiDateTime :: Location -> ForecastItem -> UTCTime
fiDateTime location fi = localTimeToUTC' tz (fiLocalTime fi)
  where
    tz = locTZ location
