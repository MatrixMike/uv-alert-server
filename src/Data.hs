{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Data where

import Control.Monad

import Data.Aeson
import Data.Function
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import GHC.Generics

import Servant

import TZ


-- Supplementary types
-- TODO: change them to something nicer

data Location = Location { city :: String
                         }
    deriving (Eq, Show, Generic, Ord)

instance FromText Location where
    fromText txt = Location <$> fromText txt

instance ToJSON Location

instance ToJSON Day where
    toJSON = toJSON . showGregorian

data UVLevel = UVLevel { uvValue :: Int }
    deriving (Eq, Ord, Show, Generic)

alertLevel :: UVLevel
alertLevel = UVLevel 3

instance ToJSON UVLevel

{-
          10        20        30        40        50        60        70       80
          *         *         *         *         *         *         *        *
Index BoM    WMO  Location            DayMonYear  UV Alert period (local time)  UVI max
0009 070014 94926 Canberra            26 09 2015  UV Alert from  8.50 to 15.00  Max:  7
-}
data Forecast = Forecast { location :: Location
                         , date :: Day
                         , alertStart :: TimeOfDay
                         , alertEnd :: TimeOfDay
                         , maxLevel :: UVLevel
                         , fcUpdated :: UTCTime
                         }
    deriving (Eq, Ord, Show, Generic)

compareUpdated :: Forecast -> Forecast -> Ordering
compareUpdated = compare `on` fcUpdated

fcTZ :: Forecast -> TimeZoneSeries
fcTZ = cityTZ . city . location

fcStartTimeUtc :: Forecast -> UTCTime
fcStartTimeUtc fc = localTimeToUTC' (fcTZ fc) $ LocalTime (date fc) (alertStart fc)

fcEndTimeUtc :: Forecast -> UTCTime
fcEndTimeUtc fc = localTimeToUTC' (fcTZ fc) $ LocalTime (date fc) (alertEnd fc)

fcDuration :: Forecast -> Int -- minutes
fcDuration fc = round (seconds / 60)
    where seconds = diffUTCTime (fcEndTimeUtc fc) (fcStartTimeUtc fc)

instance ToJSON Forecast where
    toJSON Forecast{..} = object [ "location" .= city location
                                 , "date" .= date
                                 , "alertStart" .= show alertStart
                                 , "alertEnd" .= show alertEnd
                                 , "maxLevel" .= maxLevel
                                 , "updated" .= fcUpdated
                                 ]

-- Forecast age
fcAge :: UTCTime -> Forecast -> NominalDiffTime
fcAge now fc = fromRational $ toRational $ diffUTCTime now $ fcStartTimeUtc fc

isRecent :: UTCTime -> Forecast -> Bool
isRecent now fc = fcAge now fc < (60 * 60 * 24)

data AppKey = AppKey { key :: String }

instance FromFormUrlEncoded AppKey where
    fromFormUrlEncoded = liftM (AppKey . T.unpack) . maybeToEither "key not found" . M.lookup "key" . M.fromList
        where maybeToEither _ (Just x) = Right x
              maybeToEither err Nothing = Left err
