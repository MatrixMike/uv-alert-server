{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
module Data where

import Control.Monad

import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T

import GHC.Generics

import Servant

-- Supplementary types
-- TODO: change them to something nicer

data Location = Location { city :: String
                         }
    deriving (Eq, Show, Generic)

instance FromText Location where
    fromText txt = Location <$> fromText txt

instance ToJSON Location

data Date = Date { year :: Int
                 , month :: Int
                 , day :: Int
                 }
    deriving (Eq, Show, Generic)

instance ToJSON Date

data Time = Time { hour :: Int
                 , minute :: Int
                 }
    deriving (Eq, Show, Generic)

instance ToJSON Time

data UVLevel = UVLevel Int
    deriving (Eq, Show, Generic)

instance ToJSON UVLevel

-- 0009 070014 94926 Canberra            26 09 2015  UV Alert from  8.50 to 15.00  Max:  7
data Forecast = Forecast { location :: Location
                         , date :: Date
                         , alertStart :: Time
                         , alertEnd :: Time
                         , maxLevel :: UVLevel
                         }
    deriving (Eq, Show, Generic)

instance ToJSON Forecast

data APIKey = APIKey { key :: String }

instance FromFormUrlEncoded APIKey where
    fromFormUrlEncoded = liftM (APIKey . T.unpack) . maybeToEither "key not found" . M.lookup "key" . M.fromList
        where maybeToEither _ (Just x) = Right x
              maybeToEither err Nothing = Left err
