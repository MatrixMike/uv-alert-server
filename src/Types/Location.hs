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
  , LocationTZ
  , LocationCoordinates
  , locCountry
  , locRegion
  , locCity
  , locCoordinates
  , locId
  , locTZ
  , withoutCoordinates
  , withoutTZ
  , toBaseLocation
  ) where

import Control.Lens hiding ((.=))

import Data.Aeson
import Data.String.Here
import qualified Data.Text as T
import Data.Text.Lens
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Servant

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

data LocationT coord tz = Location
  { _locCountry :: String
  , _locRegion :: String
  , _locCity :: String
  , _locCoordinates :: coord
  , _locTZ :: tz
  } deriving (Eq, Ord)

makeLenses ''LocationT

instance (Show coord, Show tz) => Show (LocationT coord tz) where
  show loc =
    [i|${loc ^. locCity}, ${loc ^. locRegion}, ${loc ^. locCountry}${coord}|]
    where
      coord :: String
      coord =
        case loc ^. locCoordinates . to show of
          "()" -> ""
          extraStr -> [i|; ${extraStr}|]

type Location = LocationT () ()

type LocationTZ = LocationT () TimeZoneSeries

type LocationCoordinates = LocationT Coordinates TimeZoneSeries

withoutCoordinates :: LocationT coord tz -> LocationT () tz
withoutCoordinates loc = loc & locCoordinates .~ ()

withoutTZ :: LocationT coord tz -> LocationT coord ()
withoutTZ loc = loc & locTZ .~ ()

toBaseLocation :: LocationT coord tz -> Location
toBaseLocation = withoutTZ . withoutCoordinates

instance FromHttpApiData Location where
  parseQueryParam txt
    -- Parse locations like "city, region, country"
   = do
    [city, region, country] <-
      mapM parseQueryParam =<<
      case T.splitOn ", " txt of
        lst@[_, _, _] -> return lst
        _ -> Left ""
    return $ Location country region city () ()

instance ToHttpApiData Location where
  toQueryParam loc =
    T.intercalate
      ", "
      [ loc ^. locCity . packed
      , loc ^. locRegion . packed
      , loc ^. locCountry . packed
      ]

_locId :: LocationT coord tz -> T.Text
_locId loc =
  normalizeValue $
  T.intercalate
    "-"
    [ loc ^. locCountry . packed
    , loc ^. locRegion . packed
    , loc ^. locCity . packed
    ]

-- | String to identify locations in pin IDs and topic names
locId :: Getter (LocationT coord tz) T.Text
locId = to _locId

locToJSON :: KeyValue t => LocationT coord tz -> [t]
locToJSON loc =
  [ "country" .= (loc ^. locCountry)
  , "region" .= (loc ^. locRegion)
  , "city" .= (loc ^. locCity)
  , "id" .= (loc ^. locId . unpacked)
  ]

instance ToJSON Location where
  toJSON loc = object $ locToJSON loc

instance ToJSON LocationCoordinates where
  toJSON loc =
    object $ locToJSON loc ++
    [ "location" .= (loc ^. locCoordinates)
    , "timezone" .= (loc ^. locTZ . to tzsTimeZone . to timeZoneName)
    ]
