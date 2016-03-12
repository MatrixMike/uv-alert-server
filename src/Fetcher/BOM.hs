module Fetcher.BOM where

{-
Fetch UV forecast from Buerau of Meteorology.

Unfortunately, this data is free for personal use but not for redistribution.
-}

import Control.Concurrent

import Control.Exception.Lifted

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Either
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Network.FTP.Client
import Network.URI

import App
import Fetcher.Base
import Types
import Types.Location
import Types.Location.Australia
import Utils


-- Data for today
todayAddress :: URI
Just todayAddress = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP007.txt"

-- Data for tomorrow (forecast)
forecastAddress :: URI
Just forecastAddress = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP026.txt"

fetchBOM :: URI -> AppM [Forecast]
fetchBOM address = do
    logStr $ "Fetching " ++ show address ++ "..."
    handle (logError address) $ do
        content <- fetchLines address
        time <- liftIO getCurrentTime
        return $ rights $ map (parseForecast time) $ lines content

fetchLines :: MonadIO m => URI -> m String
fetchLines uri = liftIO $ do
    let (Just host) = liftM uriRegName $ uriAuthority uri
    conn <- easyConnectFTP host
    loginAnon conn
    (content, _) <- getbinary conn $ uriPath uri
    return content

fetchTestContent :: MonadIO m => m String
fetchTestContent = liftIO $ readFile "src/IDYGP007.txt"

bomFetchers = [ Fetcher "BOM today" (fetchBOM todayAddress)
              , Fetcher "BOM forecast" (fetchBOM forecastAddress)
              ]

parseDate :: String -> Either String Day
parseDate str = do
    day <- readEither "day" $ stringPartT 0 2 str
    month <- readEither "month" $ stringPartT 3 2 str
    year <- readEither "year" $ stringPartT 6 4 str
    let maybeDate = fromGregorianValid year month day
    case maybeDate of
        Just date -> return date
        Nothing -> error "Invalid date"

parseTime :: String -> Either String TimeOfDay
parseTime str = do
    hour <- readEither "hour" $ stringPartT 0 2 str
    minute <- readEither "minute" $ stringPartT 3 2 str
    return $ TimeOfDay hour minute 0

bomLocation :: String -> Location
bomLocation city = Location "Australia" state city
    where state = auCityState city

-- TODO: Parsec
parseForecast :: UTCTime -> String -> Either String Forecast
parseForecast updated str = do
    let location = bomLocation $ stringPartT 18 20 str
    date <- parseDate $ stringPart 38 10 str
    tStart <- parseTime $ stringPart 64 5 str
    tEnd <- parseTime $ stringPart 73 5 str
    max <- liftM UVLevel $ readEither "UV level" $ stringPartT 84 3 str
    return $ Forecast location date tStart tEnd max updated

trim :: String -> String
trim = trimStart . trimEnd
    where trimStart = dropWhile (== ' ')
          trimEnd = reverse . trimStart . reverse

stringPart :: Int -> Int -> String -> String
stringPart start len = take len . drop start

stringPartT :: Int -> Int -> String -> String
stringPartT start len = trim . stringPart start len
