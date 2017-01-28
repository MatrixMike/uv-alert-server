{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
module Types.Location (
    Location(..),
    locCountry,
    locRegion,
    locCity,
    locId,
    locTZ,
) where

import Control.Lens hiding ((.=))

import Data.Aeson
import Data.Either
import qualified Data.Text as T
import Data.Text.Lens
import Data.Time.LocalTime.TimeZone.Series

import GHC.Generics hiding (to)

import Servant

import Types.Location.Australia
import Types.Location.Japan
import Types.Location.USA
import Utils


data Location = Location { _locCountry :: String
                         , _locRegion :: String
                         , _locCity :: String
                         }
    deriving (Eq, Show, Generic, Ord)
makeLenses ''Location

instance FromHttpApiData Location where
  parseQueryParam txt
    -- Parse locations like "city, region, country"
   = do
    [city, region, country] <-
      mapM parseQueryParam =<<
      case T.splitOn ", " txt of
        lst@[_, _, _] -> return lst
        _ -> Left ""
    return $ Location country region city

instance ToHttpApiData Location where
  toQueryParam loc =
    T.intercalate
      ", "
      [ loc ^. locCity . packed
      , loc ^. locRegion . packed
      , loc ^. locCountry . packed
      ]

_locId :: Location -> T.Text
_locId loc = normalizeValue $ T.intercalate "-" [ loc ^. locCountry . packed
                                                , loc ^. locRegion . packed
                                                , loc ^. locCity . packed
                                                ]

-- String to identify locations in pin IDs and topic names
locId :: Getter Location T.Text
locId = to _locId

instance ToJSON Location where
    toJSON loc = object [ "country" .= (loc ^. locCountry)
                        , "region" .= (loc ^. locRegion)
                        , "city" .= (loc ^. locCity)
                        , "id" .= (loc ^. locId . unpacked)
                        ]

-- FIXME: disallow creating locations if the time zone is unknown
locTZ :: Location -> TimeZoneSeries
locTZ (Location "Australia" state _) = auStateTZ state
locTZ (Location "Japan" _ _) = japanTZ
locTZ loc@(Location "USA" state city) = usTZ city state
locTZ loc = error $ "Unknown time zone for location " ++ show loc
