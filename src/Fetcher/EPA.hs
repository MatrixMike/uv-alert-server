{-# LANGUAGE OverloadedStrings #-}
module Fetcher.EPA where

{- Fetch USA data from EPA API. -}

import Control.Exception.Lifted
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

import App
import Fetcher.Base
import Fetcher.EPA.Cities
import Fetcher.HTTP
import Types
import Types.Location
import Types.Location.USA
import Utils


epaFetcher :: Fetcher
epaFetcher = Fetcher "EPA" fetchEpa

fetchEpa :: AppM [Forecast]
fetchEpa = do
    manager <- liftIO $ newManager defaultManagerSettings
    liftM concat $ forM usCities $ \(city, state) -> do
        logStr $ "Fetching graph for " ++ city ++ ", " ++ state ++ "..."
        let address = forecastAddress city state
        handle (logError address) $ do
            responseStr <- fetchHTTP manager address
            case decode $ LBS.fromStrict responseStr of
                Just response -> do
                    time <- liftIO getCurrentTime
                    return $ maybeToList $ buildForecast city state response time
                Nothing -> do
                    logStr $ "Error parsing JSON: " ++ show responseStr
                    return []

forecastAddress city state = "https://iaspub.epa.gov/enviro/efservice/getEnvirofactsUVHOURLY/CITY/" ++ city ++ "/STATE/" ++ abbr ++ "/JSON"
    where abbr = usStateAbbreviation state

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
fiDateTime :: String -> String -> ForecastItem -> UTCTime
fiDateTime city state fi = localTimeToUTC' (usTZ city state) (fiLocalTime fi)

buildForecast :: String -> String -> [ForecastItem] -> UTCTime -> Maybe Forecast
buildForecast _ _ [] _ = Nothing
buildForecast city state items@(firstItem:_) updated = do
    let tz = usTZ city state
    let localDayTime = localTimeOfDay . utcToLocalTime' tz
    let levels = map fiLevel items
    let maxlevel = maximum levels
    guard $ isDangerous maxlevel
    let firstTime = fiDateTime city state firstItem
    astart <- liftM (flip addHours firstTime) (firstAlertTime levels)
    aend <- liftM (flip addHours firstTime) (lastAlertTime levels)
    return Forecast { _fcLocation = Location "USA" city state
                    , _fcDate = utctDay astart
                    , _fcAlertStart = localDayTime astart
                    , _fcAlertEnd = localDayTime aend
                    , _fcMaxLevel = maxlevel
                    , _fcUpdated = updated
                    }

addHours :: Float -> UTCTime -> UTCTime
addHours hours = addUTCTime $ fromRational $ toRational $ hours * 60 * 60

maybeSplitHead :: [a] -> Maybe (a, [a])
maybeSplitHead [] = Nothing
maybeSplitHead (a:as) = Just (a, as)

firstAlertTime :: [UVLevel] -> Maybe Float
firstAlertTime ls = do
        (l1, ls') <- maybeSplitHead ls
        if isDangerous l1 then return 0 else do
            (l2, _) <- maybeSplitHead ls'
            if isDangerous l2 then return $ extrapolateUV l1 l2 alertLevel
                                else do
                                    alertTime <- firstAlertTime ls'
                                    return $ alertTime + 1
    where extrapolateUV :: UVLevel -> UVLevel -> UVLevel -> Float
          extrapolateUV v1 v2 v = extrapolate (uvToFloat v1, 0) (uvToFloat v2, 1) (uvToFloat v)
          uvToFloat v = v ^. uvValue . to toInteger . to fromInteger

lastAlertTime :: [UVLevel] -> Maybe Float
lastAlertTime = undefined
