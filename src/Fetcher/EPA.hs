{-# LANGUAGE OverloadedStrings #-}

{-| Fetch USA data from EPA API. -}
module Fetcher.EPA where

import Control.Arrow
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
epaFetcher = Fetcher "EPA" fetchEpa cities

fetchEpa :: AppM [Forecast]
fetchEpa =
  fmap concat $
  forM cities $ \location -> do
    logStr $ "Fetching forecast for " ++ show location ++ "..."
    let address = forecastAddress location
    logErrors address $ do
      response <- parseRequest address >>= httpJSON
      time <- liftIO getCurrentTime
      let measurements =
            map (fiDateTime location &&& fiLevel) (responseBody response)
      return $ maybeToList $ buildForecast location time measurements

forecastAddress :: LocationT coord tz -> String
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
      level <- UVLevel <$> v .: "UV_VALUE"
      time <- (v .: "DATE_TIME") >>= parseLocalTime
      return $ ForecastItem time level
    where
      parseLocalTime = parseTimeM False defaultTimeLocale "%b/%d/%Y %I %P"

-- Parse a date from the forecast in a format: MAR/17/2016 11 PM
fiDateTime :: LocationT coord TimeZoneSeries -> ForecastItem -> UTCTime
fiDateTime location fi = localTimeToUTC' tz (fiLocalTime fi)
  where
    tz = location ^. locTZ
