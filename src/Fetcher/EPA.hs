{-# LANGUAGE OverloadedStrings #-}
module Fetcher.EPA where

{- Fetch USA data from EPA API. -}

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Fetcher.Base
import Fetcher.EPA.Cities
import Fetcher.HTTP
import Types
import Types.Config
import Types.Location
import Types.Location.USA
import Utils


epaFetcher :: Fetcher
epaFetcher = Fetcher "EPA" fetchEpa usLocations

fetchEpa :: AppM [Forecast]
fetchEpa = do
    liftM concat $ forM usLocations $ \location -> do
        logStr $ "Fetching forecast for " ++ show location ++ "..."
        let address = forecastAddress location
        logErrors address $ do
            responseStr <- fetchHTTP address
            case decode $ LBS.fromStrict responseStr of
                Just response -> do
                    time <- liftIO getCurrentTime
                    let measurements = map (\fi -> (fiDateTime location fi, fiLevel fi)) response
                    return $ maybeToList $ buildForecast location time measurements
                Nothing -> do
                    logStr $ "Error parsing JSON: " ++ show responseStr
                    return []

forecastAddress location = "https://iaspub.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/CITY/" ++ city ++ "/STATE/" ++ abbr ++ "/JSON"
    where city = location ^. locCity
          abbr = location ^. locRegion . to usStateAbbreviation

data ForecastItem = ForecastItem { fiLocalTime :: LocalTime
                                 , fiLevel :: UVLevel
                                 }

instance FromJSON ForecastItem where
    parseJSON (Object v) = do
            level <- liftM UVLevel $ v .: "UV_VALUE"
            time <- (v .: "DATE_TIME") >>= parseLocalTime
            return $ ForecastItem time level
        where parseLocalTime = parseTimeM False defaultTimeLocale "%b/%d/%Y %I %P"

-- Parse a date from the forecast in a format: MAR/17/2016 11 PM
fiDateTime :: Location -> ForecastItem -> UTCTime
fiDateTime location fi = localTimeToUTC' tz (fiLocalTime fi)
    where tz = usTZ (location ^. locCity) (location ^. locRegion)
