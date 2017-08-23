{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens hiding ((.=))

import Control.Arrow

import Data.Aeson
import Data.Ord
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import GHC.Generics (Generic)

import Web.FormUrlEncoded

import Types.Location
import Utils

-- Supplementary types
newtype UVLevel = UVLevel
  { _uvValue :: Int
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''UVLevel

alertLevel :: UVLevel
alertLevel = UVLevel 3

isDangerous :: UVLevel -> Bool
isDangerous = (>= alertLevel)

instance ToJSON UVLevel where
  toJSON level = toJSON $ level ^. uvValue

-- | A single event of UV index exceeding the safe levels.
data Alert = Alert
  { _alertStart :: TimeOfDay
  , _alertEnd :: TimeOfDay
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''Alert

instance ToJSON Alert where
  toJSON alert =
    object
      ["start" .= show (alert ^. alertStart), "end" .= show (alert ^. alertEnd)]

-- | A list of all times the UV index is dangerous for the day.
data Forecast = Forecast
  { _fcLocation :: Location
  , _fcDate :: Day
  , _fcAlerts :: [Alert]
  , _fcMaxLevel :: UVLevel
  , _fcUpdated :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''Forecast

compareUpdated :: Forecast -> Forecast -> Ordering
compareUpdated = comparing $ view fcUpdated

fcTZ :: Forecast -> TimeZoneSeries
fcTZ fc = fc ^. fcLocation . to locTZ

fcTime :: Forecast -> TimeOfDay -> UTCTime
fcTime fc = localTimeToUTC' (fcTZ fc) . LocalTime (fc ^. fcDate)

fcAlertStartTime :: Forecast -> Alert -> UTCTime
fcAlertStartTime fc alert = fcTime fc (alert ^. alertStart)

fcAlertEndTime :: Forecast -> Alert -> UTCTime
fcAlertEndTime fc alert = fcTime fc (alert ^. alertEnd)

instance ToJSON Forecast where
  toJSON fc =
    object
      [ "location" .= (fc ^. fcLocation . locCity)
      , "date" .= (fc ^. fcDate . to showGregorian)
      , "alerts" .= (fc ^. fcAlerts)
      , "maxLevel" .= (fc ^. fcMaxLevel)
      , "updated" .= (fc ^. fcUpdated)
      ]

-- Forecast age, counted from the end time of last alert
fcAge :: UTCTime -> Forecast -> NominalDiffTime
fcAge now fc =
  fromRational $ toRational $ diffUTCTime now $ maximum $
  map (fcAlertEndTime fc) $
  fc ^.
  fcAlerts

isRecent :: UTCTime -> Forecast -> Bool
isRecent now fc = fcAge now fc < (60 * 60 * 12)

type Measurement = (UTCTime, UVLevel)

-- Build a forecast from a number of measurements
buildForecast :: Location -> UTCTime -> [Measurement] -> Maybe Forecast
buildForecast location updated measurements = do
  let tz = locTZ location
  let localDayTime = localTimeOfDay . utcToLocalTime' tz
  let alertTimes = alertIntervals measurements
  firstAlert <- fst <$> listToMaybe alertTimes
  maxlevel <- maybeMaximum $ map snd measurements
  return
    Forecast
    { _fcLocation = location
    , _fcDate = (localDay . utcToLocalTime' tz) firstAlert
    , _fcAlerts =
        [ Alert (localDayTime astart) (localDayTime aend)
        | (astart, aend) <- alertTimes
        ]
    , _fcMaxLevel = maxlevel
    , _fcUpdated = updated
    }

timeToDiffTime :: UTCTime -> NominalDiffTime
timeToDiffTime = flip diffUTCTime tconst

diffTimeToTime :: NominalDiffTime -> UTCTime
diffTimeToTime = flip addUTCTime tconst

tconst :: UTCTime
tconst = UTCTime (fromGregorian 2001 1 1) 0

alertIntervals :: [Measurement] -> [(UTCTime, UTCTime)]
alertIntervals =
  fmap (first diffTimeToTime . second diffTimeToTime) .
  findIntervals (uvToFloat alertLevel) .
  map (second uvToFloat . first timeToDiffTime)
  where
    uvToFloat :: UVLevel -> Double
    uvToFloat v = v ^. uvValue . to toInteger . to fromInteger

newtype AppKey = AppKey
  { akKey :: String
  }

instance FromForm AppKey where
  fromForm f = AppKey <$> parseUnique "key" f
