{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Types.Location
  ( Coordinates(..)
  , latitude
  , longitude
  , latlon
  , LocationT(..)
  , Location
  , LocationCoordinates
  , locCountry
  , locRegion
  , locCity
  , locExtra
  , locId
  , locTZ
  , withoutCoordinates
  ) where

import Control.Lens hiding ((.=))

import Data.Aeson
import Data.String.Here
import qualified Data.Text as T
import Data.Text.Lens
import Data.Time.LocalTime.TimeZone.Series

import Servant

import Types.Location.Australia
import Types.Location.Japan
import Types.Location.USA
import Utils

data Coordinates = Coordinates
  { _latitude :: Double
  , _longitude :: Double
  } deriving (Eq, Ord)

makeLenses ''Coordinates

instance Show Coordinates where
  show coo = [i|latlon ${coo ^. latitude} ${coo ^. longitude}|]

latlon :: Double -> Double -> Coordinates
latlon lat lon = Coordinates { _latitude = lat, _longitude = lon }

instance ToJSON Coordinates where
  toJSON coo = object ["lat" .= (coo ^. latitude), "lon" .= (coo ^. longitude)]

data LocationT extra = Location
  { _locCountry :: String
  , _locRegion :: String
  , _locCity :: String
  , _locExtra :: extra
  } deriving (Eq, Ord)

makeLenses ''LocationT

instance Show extra => Show (LocationT extra) where
  show loc =
    [i|${loc ^. locCity}, ${loc ^. locRegion}, ${loc ^. locCountry}${extra}|]
    where
      extra :: String
      extra =
        case loc ^. locExtra . to show of
          "()" -> ""
          extraStr -> [i|; ${extraStr}|]

type Location = LocationT ()

type LocationCoordinates = LocationT Coordinates

withoutCoordinates :: LocationT extra -> Location
withoutCoordinates loc = loc & locExtra .~ ()

instance FromHttpApiData Location where
  parseQueryParam txt
    -- Parse locations like "city, region, country"
   = do
    [city, region, country] <-
      mapM parseQueryParam =<<
      case T.splitOn ", " txt of
        lst@[_, _, _] -> return lst
        _ -> Left ""
    return $ Location country region city ()

instance ToHttpApiData Location where
  toQueryParam loc =
    T.intercalate
      ", "
      [ loc ^. locCity . packed
      , loc ^. locRegion . packed
      , loc ^. locCountry . packed
      ]

_locId :: LocationT extra -> T.Text
_locId loc =
  normalizeValue $
  T.intercalate
    "-"
    [ loc ^. locCountry . packed
    , loc ^. locRegion . packed
    , loc ^. locCity . packed
    ]

-- | String to identify locations in pin IDs and topic names
locId :: Getter (LocationT extra) T.Text
locId = to _locId

locToJSON :: KeyValue t => LocationT extra -> [t]
locToJSON loc =
  [ "country" .= (loc ^. locCountry)
  , "region" .= (loc ^. locRegion)
  , "city" .= (loc ^. locCity)
  , "id" .= (loc ^. locId . unpacked)
  ]

instance ToJSON Location where
  toJSON loc = object $ locToJSON loc

instance ToJSON LocationCoordinates where
  toJSON loc = object $ locToJSON loc ++ ["location" .= (loc ^. locExtra)]

-- FIXME: disallow creating locations if the time zone is unknown
locTZ :: LocationT extra -> TimeZoneSeries
locTZ loc =
  case loc ^. locCountry of
    "Australia" -> loc ^. locRegion . to auStateTZ
    "Japan" -> japanTZ
    "USA" -> usTZ (loc ^. locCity) (loc ^. locRegion)
    _ -> error $ "Unknown time zone for location " ++ show (withoutCoordinates loc)
