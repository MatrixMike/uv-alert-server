{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}
module Types where

import Control.Lens hiding ((.=))

import Control.Monad

import Data.Aeson
import Data.Function
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import GHC.Generics (Generic)

import Servant

import Types.Location
import Utils


-- Supplementary types

data UVLevel = UVLevel { _uvValue :: Int }
    deriving (Eq, Ord, Show, Generic)
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
    toJSON alert = object [ "start" .= show (alert ^. alertStart)
                          , "end" .= show (alert ^. alertEnd) ]

-- | A list of all times the UV index is dangerous for the day.
data Forecast = Forecast { _fcLocation :: Location
                         , _fcDate :: Day
                         , _fcAlerts :: [Alert]
                         , _fcMaxLevel :: UVLevel
                         , _fcUpdated :: UTCTime
                         }
    deriving (Eq, Ord, Show, Generic)
makeLenses ''Forecast

compareUpdated :: Forecast -> Forecast -> Ordering
compareUpdated = compare `on` (view fcUpdated)

fcTZ :: Forecast -> TimeZoneSeries
fcTZ fc = fc ^. fcLocation . to locTZ

fcTime :: Forecast -> TimeOfDay -> UTCTime
fcTime fc = localTimeToUTC' (fcTZ fc) . LocalTime (fc ^. fcDate)

fcAlertStartTime :: Forecast -> Alert -> UTCTime
fcAlertStartTime fc alert = fcTime fc (alert ^. alertStart)

fcAlertEndTime :: Forecast -> Alert -> UTCTime
fcAlertEndTime fc alert = fcTime fc (alert ^. alertEnd)

instance ToJSON Forecast where
    toJSON fc = object [ "location" .= (fc ^. fcLocation . locCity)
                       , "date" .= (fc ^. fcDate . to showGregorian)
                       , "alerts" .= (fc ^. fcAlerts)
                       , "maxLevel" .= (fc ^. fcMaxLevel)
                       , "updated" .= (fc ^. fcUpdated)
                       ]

-- Forecast age, counted from the end time of last alert
fcAge :: UTCTime -> Forecast -> NominalDiffTime
fcAge now fc =
    fromRational $
    toRational $
    diffUTCTime now $ maximum $ map (fcAlertEndTime fc) $ fc ^. fcAlerts

isRecent :: UTCTime -> Forecast -> Bool
isRecent now fc = fcAge now fc < (60 * 60 * 24)

type Measurement = (UTCTime, UVLevel)

-- Build a forecast from a number of measurements
buildForecast :: Location -> UTCTime -> [Measurement] -> Maybe Forecast
buildForecast location updated measurements = do
    let tz = locTZ location
    let localDayTime = localTimeOfDay . utcToLocalTime' tz
    astart <- firstAlertTime measurements
    aend <- lastAlertTime measurements
    maxlevel <- maybeMaximum $ map snd measurements
    return Forecast { _fcLocation = location
                    , _fcDate = (localDay . utcToLocalTime' tz) astart
                    , _fcAlerts = [Alert (localDayTime astart) (localDayTime aend)]
                    , _fcMaxLevel = maxlevel
                    , _fcUpdated = updated
                    }

firstAlertTime :: [Measurement] -> Maybe UTCTime
firstAlertTime ls = do
    (l1, ls') <- maybeSplitHead ls
    if isDangerous (snd l1)
        then return (fst l1)
        else do
            (l2, _) <- maybeSplitHead ls'
            if isDangerous (snd l2)
                then return $ extrapolateUV l1 l2
                else firstAlertTime ls'

lastAlertTime :: [Measurement] -> Maybe UTCTime
lastAlertTime = firstAlertTime . reverse

extrapolateUV :: Measurement -> Measurement -> UTCTime
extrapolateUV (t1, v1) (t2, v2) = addUTCTime extrapolated t1
    where uvToFloat v = v ^. uvValue . to toInteger . to fromInteger
          dt1 = diffUTCTime t1 t1
          dt2 = diffUTCTime t2 t1
          extrapolated = extrapolate (uvToFloat v1, dt1) (uvToFloat v2, dt2) (uvToFloat alertLevel)

data AppKey = AppKey { akKey :: String }

instance FromFormUrlEncoded AppKey where
    fromFormUrlEncoded = liftM (AppKey . T.unpack) . maybeToEither "key not found" . M.lookup "key" . M.fromList
        where maybeToEither _ (Just x) = Right x
              maybeToEither err Nothing = Left err
