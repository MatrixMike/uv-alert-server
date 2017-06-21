{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Fetcher.Arpansa where

{- Fetch UV alert data from ARPANSA. -}

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Network.HTTP.Client
import Network.HTTP.Simple

import Fetcher.Base
import Types
import Types.Config
import Types.Location


data ArpansaLocation = ArpansaLocation { _alLocation :: Location
                                       , _alLongitude :: T.Text
                                       , _alLatitude :: T.Text
                                       } deriving (Eq, Ord)
makeLenses ''ArpansaLocation

instance Show ArpansaLocation where
  show l = "ArpansaLocation (" ++ l ^. alLocation . locCity ++ ", " ++ show (l ^. alLatitude) ++ ", " ++ show (l ^. alLongitude) ++ ")"

addresses :: [ArpansaLocation]
addresses = map makeLocation [ (sa, "Adelaide", "-34.92", "138.62")
                             , (nt, "Alice Springs", "-23.7", "133.8")
                             , (qld, "Brisbane", "-27.45", "153.03")
                             , (act, "Canberra", "-35.31", "149.2")
                             , (nt, "Darwin", "-12.43", "130.89")
                             , (tas, "Kingston", "-42.99", "147.29")
                             , (vic, "Melbourne", "-37.73", "145.1")
                             , (nsw, "Newcastle", "-32.9", "151.72")
                             , (wa, "Perth", "-31.92", "115.96")
                             , (nsw, "Sydney", "-34.04", "151.1")
                             , (qld, "Townsville", "-19.33", "146.76")
                             ]
    where makeLocation (state, town, lat, lon) = ArpansaLocation (Location "Australia" state town) lon lat
          act = "Australian Capital Territory"
          nsw = "New South Wales"
          nt = "Northern Territory"
          qld = "Queensland"
          sa = "South Australia"
          tas = "Tasmania"
          vic = "Victoria"
          wa = "Western Australia"

data ForecastPointT time = ForecastPointT
  { _fpTime :: time
  , _fpForecast :: Maybe UVLevel
  , _fpMeasured :: Maybe UVLevel
  } deriving (Show, Functor)
makeLenses ''ForecastPointT

fpMeasurement :: ForecastPointT time -> (time, UVLevel)
fpMeasurement pt =
  (pt ^. fpTime, fromMaybe (UVLevel 0) (pt ^. fpMeasured <|> pt ^. fpForecast))

parseArpansaTime :: Monad m => String -> m LocalTime
parseArpansaTime = parseTimeM False defaultTimeLocale "%F %R"

instance FromJSON (ForecastPointT LocalTime) where
  parseJSON =
    withObject "forecast point" $ \o -> do
      date <- o .: "Date" >>= parseArpansaTime
      forecast <- fmap UVLevel <$> o .:? "Forecast"
      measured <- fmap UVLevel <$> o .:? "Measured"
      return $ ForecastPointT date forecast measured

data ArpansaForecastT time = ArpansaForecastT
  { _afPoints :: [ForecastPointT time]
  } deriving (Show, Functor)
makeLenses ''ArpansaForecastT

instance FromJSON (ArpansaForecastT LocalTime) where
  parseJSON =
    withObject "forecast" $ \o -> ArpansaForecastT <$> o .: "GraphData"

fetchArpansa :: AppM [Forecast]
fetchArpansa = do
  baseRequest <- parseRequest "https://uvdata.arpansa.gov.au/api/uvlevel/"
  fmap catMaybes $ flip traverse addresses $ \loc -> do
    logStr $ "Fetching graph for " ++ loc ^. alLocation . locCity ++ "..."
    time <- liftIO getCurrentTime
    let tz = loc ^. alLocation . to locTZ
    logErrors loc $ do
      let request =
            baseRequest &
            setRequestQueryString
              [ ("longitude", Just $ loc ^. alLongitude . to T.encodeUtf8)
              , ("latitude", Just $ loc ^. alLatitude . to T.encodeUtf8)
              , ( "date"
                , Just $ T.encodeUtf8 $ T.pack $
                  formatTime defaultTimeLocale "%F" $
                  utcToLocalTime' tz time)
              ]
      graphData <- responseBody <$> httpJSON request
      let graphDataUtc = fmap (localTimeToUTC' tz) graphData
      let graphPoints = graphDataUtc ^. afPoints
      return $ buildForecast (loc ^. alLocation) time $
        map fpMeasurement graphPoints

arpansaFetcher :: Fetcher
arpansaFetcher = Fetcher "ARPANSA" fetchArpansa $ map (view alLocation) addresses
