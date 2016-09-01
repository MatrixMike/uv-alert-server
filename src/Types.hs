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
-- TODO: change them to something nicer

data UVLevel = UVLevel { _uvValue :: Int }
    deriving (Eq, Ord, Show, Generic)
makeLenses ''UVLevel

alertLevel :: UVLevel
alertLevel = UVLevel 3

isDangerous :: UVLevel -> Bool
isDangerous = (>= alertLevel)

instance ToJSON UVLevel where
    toJSON level = toJSON $ level ^. uvValue

{-
          10        20        30        40        50        60        70       80
          *         *         *         *         *         *         *        *
Index BoM    WMO  Location            DayMonYear  UV Alert period (local time)  UVI max
0009 070014 94926 Canberra            26 09 2015  UV Alert from  8.50 to 15.00  Max:  7
-}
data Forecast = Forecast { _fcLocation :: Location
                         , _fcDate :: Day
                         , _fcAlertStart :: TimeOfDay
                         , _fcAlertEnd :: TimeOfDay
                         , _fcMaxLevel :: UVLevel
                         , _fcUpdated :: UTCTime
                         }
    deriving (Eq, Ord, Show, Generic)
makeLenses ''Forecast

compareUpdated :: Forecast -> Forecast -> Ordering
compareUpdated = compare `on` (view fcUpdated)

fcTZ :: Forecast -> TimeZoneSeries
fcTZ fc = fc ^. fcLocation . to locTZ

fcStartTimeUtc :: Forecast -> UTCTime
fcStartTimeUtc fc = localTimeToUTC' (fcTZ fc) $ LocalTime (fc ^. fcDate) (fc ^. fcAlertStart)

fcEndTimeUtc :: Forecast -> UTCTime
fcEndTimeUtc fc = localTimeToUTC' (fcTZ fc) $ LocalTime (fc ^. fcDate) (fc ^. fcAlertEnd)

fcDuration :: Forecast -> Int -- minutes
fcDuration fc = round (seconds / 60)
    where seconds = diffUTCTime (fcEndTimeUtc fc) (fcStartTimeUtc fc)

instance ToJSON Forecast where
    toJSON fc = object [ "location" .= (fc ^. fcLocation . locCity)
                       , "date" .= (fc ^. fcDate . to showGregorian)
                       , "alertStart" .= show (fc ^. fcAlertStart)
                       , "alertEnd" .= show (fc ^. fcAlertEnd)
                       , "maxLevel" .= (fc ^. fcMaxLevel)
                       , "updated" .= (fc ^. fcUpdated)
                       ]

-- Forecast age
fcAge :: UTCTime -> Forecast -> NominalDiffTime
fcAge now fc = fromRational $ toRational $ diffUTCTime now $ fcStartTimeUtc fc

isRecent :: UTCTime -> Forecast -> Bool
isRecent now fc = fcAge now fc < (60 * 60 * 24)

-- Build a forecast from a number of measurements
buildForecast :: Location -> UTCTime -> [(UTCTime, UVLevel)] -> Maybe Forecast
buildForecast _ _ [] = Nothing
buildForecast location updated items@(firstItem:_) = do
    let tz = locTZ location
    let localDayTime = localTimeOfDay . utcToLocalTime' tz
    let levels = map snd items
    let maxlevel = maximum levels
    guard $ isDangerous maxlevel
    let firstTime = fst firstItem
    astart <- liftM (flip addHours firstTime) (firstAlertTime levels)
    aend <- liftM (flip addHours firstTime) (lastAlertTime levels)
    return Forecast { _fcLocation = location
                    , _fcDate = (localDay . utcToLocalTime' tz) astart
                    , _fcAlertStart = localDayTime astart
                    , _fcAlertEnd = localDayTime aend
                    , _fcMaxLevel = maxlevel
                    , _fcUpdated = updated
                    }

addHours :: Float -> UTCTime -> UTCTime
addHours hours = addUTCTime $ fromRational $ toRational $ hours * 60 * 60

firstAlertTime :: [UVLevel] -> Maybe Float
firstAlertTime ls = do
    (l1, ls') <- maybeSplitHead ls
    if isDangerous l1 then return 0 else do
        (l2, _) <- maybeSplitHead ls'
        if isDangerous l2 then return $ extrapolateUV l1 l2
                            else do
                                alertTime <- firstAlertTime ls'
                                return $ alertTime + 1

lastAlertTime :: [UVLevel] -> Maybe Float
lastAlertTime ls = do
    alertTime <- firstAlertTime $ reverse ls
    return $ fromInteger (toInteger (length ls - 1)) - alertTime

extrapolateUV :: UVLevel -> UVLevel -> Float
extrapolateUV v1 v2 = extrapolate (uvToFloat v1, 0) (uvToFloat v2, 1) (uvToFloat alertLevel)
    where uvToFloat v = v ^. uvValue . to toInteger . to fromInteger

data AppKey = AppKey { akKey :: String }

instance FromFormUrlEncoded AppKey where
    fromFormUrlEncoded = liftM (AppKey . T.unpack) . maybeToEither "key not found" . M.lookup "key" . M.fromList
        where maybeToEither _ (Just x) = Right x
              maybeToEither err Nothing = Left err
