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


-- Supplementary types
-- TODO: change them to something nicer

data UVLevel = UVLevel { _uvValue :: Int }
    deriving (Eq, Ord, Show, Generic)
makeLenses ''UVLevel

alertLevel :: UVLevel
alertLevel = UVLevel 3

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

data AppKey = AppKey { akKey :: String }

instance FromFormUrlEncoded AppKey where
    fromFormUrlEncoded = liftM (AppKey . T.unpack) . maybeToEither "key not found" . M.lookup "key" . M.fromList
        where maybeToEither _ (Just x) = Right x
              maybeToEither err Nothing = Left err
