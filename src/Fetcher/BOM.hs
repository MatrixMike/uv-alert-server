module Fetcher.BOM where

{-|
Fetch UV forecast from Buerau of Meteorology.

Unfortunately, this data is free for personal use but not for redistribution.
-}
import Control.Monad.IO.Class

import Data.Either
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Network.FTP.Client
import Network.URI

import Fetcher.Base
import Types
import Types.Config
import Types.Location
import Types.Location.Australia
import Utils

-- Data for today
todayAddress :: URI
Just todayAddress = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP007.txt"

-- Data for tomorrow (forecast)
forecastAddress :: URI
Just forecastAddress =
  parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP026.txt"

fetchBOM :: URI -> AppM [Forecast]
fetchBOM address = do
  logStr $ "Fetching " ++ show address ++ "..."
  logErrors address $ do
    content <- fetchLines address
    time <- liftIO getCurrentTime
    return $ rights $ map (parseForecast time) $ lines content

fetchLines :: MonadIO m => URI -> m String
fetchLines uri =
  liftIO $ do
    let (Just host) = uriRegName <$> uriAuthority uri
    conn <- easyConnectFTP host
    _ <- loginAnon conn
    (content, _) <- getbinary conn $ uriPath uri
    return content

-- FIXME: BOM fetcher doesn't have a list of locations
bomFetchers :: [Fetcher]
bomFetchers =
  [ Fetcher "BOM today" (fetchBOM todayAddress) []
  , Fetcher "BOM forecast" (fetchBOM forecastAddress) []
  ]

parseDate :: String -> Either String Day
parseDate str = do
  day <- readEither "day" $ stringPartT 0 2 str
  month <- readEither "month" $ stringPartT 3 2 str
  year <- readEither "year" $ stringPartT 6 4 str
  maybeToEither ("Invalid date: " ++ str) $ fromGregorianValid year month day

parseTime :: String -> Either String TimeOfDay
parseTime str = do
  hour <- readEither "hour" $ stringPartT 0 2 str
  minute <- readEither "minute" $ stringPartT 3 2 str
  return $ TimeOfDay hour minute 0

bomLocation :: String -> Location
bomLocation city = Location "Australia" state city
  where
    state = auCityState city

-- TODO: Parsec
parseForecast :: UTCTime -> String -> Either String Forecast
parseForecast updated str = do
  let location = bomLocation $ stringPartT 18 20 str
  date <- parseDate $ stringPart 38 10 str
  tStart <- parseTime $ stringPart 64 5 str
  tEnd <- parseTime $ stringPart 73 5 str
  maxLevel <- fmap UVLevel $ readEither "UV level" $ stringPartT 84 3 str
  return
    Forecast
    { _fcLocation = location
    , _fcDate = date
    , _fcAlerts = [Alert tStart tEnd]
    , _fcMaxLevel = maxLevel
    , _fcUpdated = updated
    }

trim :: String -> String
trim = trimStart . trimEnd
  where
    trimStart = dropWhile (== ' ')
    trimEnd = reverse . trimStart . reverse

stringPart :: Int -> Int -> String -> String
stringPart start len = take len . drop start

stringPartT :: Int -> Int -> String -> String
stringPartT start len = trim . stringPart start len
