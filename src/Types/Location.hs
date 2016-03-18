{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
module Types.Location (
    Location(..),
    locCountry,
    locRegion,
    locCity,
    locTZ,
) where

import Control.Lens hiding ((.=))

import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime.TimeZone.Series

import GHC.Generics

import Servant

import Types.Location.Australia
import Types.Location.USA


data Location = Location { _locCountry :: String
                         , _locRegion :: String
                         , _locCity :: String
                         }
    deriving (Eq, Show, Generic, Ord)
makeLenses ''Location

instance FromText Location where
    -- Parse locations like "city, region, country"
    fromText txt = do
        [city, region, country] <- mapM fromText =<< case T.splitOn ", " txt of
                                                       lst@[_, _, _] -> Just lst
                                                       _ -> Nothing
        return $ Location country region city

instance ToJSON Location where
    toJSON loc = object [ "country" .= (loc ^. locCountry)
                        , "region" .= (loc ^. locRegion)
                        , "city" .= (loc ^. locCity)
                        ]

-- FIXME: disallow creating locations if the time zone is unknown
locTZ :: Location -> TimeZoneSeries
locTZ (Location "Australia" state _) = auStateTZ state
locTZ loc@(Location "USA" city state) = usTZ city state
locTZ loc = error $ "Unknown time zone for location " ++ show loc
