{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-| Fetch UV alert data from ARPANSA. -}
module Fetcher.Australia.Arpansa where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Decimal
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series
import Data.Traversable

import Network.HTTP.Client
import Network.HTTP.Simple

import Fetcher.Base
import Fetcher.Australia.Common
import Types
import Types.Config
import Types.Location

roundLocation :: Double -> BS.ByteString
roundLocation = T.encodeUtf8 . T.pack . show @Decimal . realFracToDecimal 2

data ForecastPointT time = ForecastPointT
  { _fpTime :: time
  , _fpForecast :: Maybe Double
  , _fpMeasured :: Maybe Double
  } deriving (Show, Functor)

makeLenses ''ForecastPointT

fpUVLevel :: ForecastPointT time -> Maybe UVLevel
fpUVLevel pt = fmap (UVLevel . round) (pt ^. fpMeasured <|> pt ^. fpForecast)

fpMeasurement :: ForecastPointT time -> (time, UVLevel)
fpMeasurement pt =
  (pt ^. fpTime, fromMaybe (UVLevel 0) (fpUVLevel pt))

parseArpansaTime :: Monad m => String -> m LocalTime
parseArpansaTime = parseTimeM False defaultTimeLocale "%F %R"

instance FromJSON (ForecastPointT LocalTime) where
  parseJSON =
    withObject "forecast point" $ \o -> do
      date <- o .: "Date" >>= parseArpansaTime
      forecast <- o .:? "Forecast"
      measured <- o .:? "Measured"
      return $ ForecastPointT date forecast measured

newtype ArpansaForecastT time = ArpansaForecastT
  { _afPoints :: [ForecastPointT time]
  } deriving (Show, Functor)

makeLenses ''ArpansaForecastT

instance FromJSON (ArpansaForecastT LocalTime) where
  parseJSON =
    withObject "forecast" $ \o -> ArpansaForecastT <$> o .: "GraphData"

fetchArpansa :: AppM [Forecast]
fetchArpansa = do
  baseRequest <- parseRequest "https://uvdata.arpansa.gov.au/api/uvlevel/"
  fmap catMaybes $ for auCities $ \loc -> do
    logStr $ "Fetching graph for " ++ loc ^. locCity ++ "..."
    time <- liftIO getCurrentTime
    let tz = loc ^. to locTZ
    logErrors loc $ do
      let request =
            baseRequest &
            setRequestQueryString
              [ ("longitude", Just $ loc ^. locExtra . longitude . to roundLocation)
              , ("latitude", Just $ loc ^. locExtra . latitude . to roundLocation)
              , ( "date"
                , Just $ T.encodeUtf8 $ T.pack $
                  formatTime defaultTimeLocale "%F" $
                  utcToLocalTime' tz time)
              ]
      graphData <- responseBody <$> httpJSON request
      let graphDataUtc = fmap (localTimeToUTC' tz) graphData
      let graphPoints = graphDataUtc ^. afPoints
      return $ buildForecast loc time $
        map fpMeasurement graphPoints

arpansaFetcher :: Fetcher
arpansaFetcher =
  Fetcher "ARPANSA" fetchArpansa auCities
