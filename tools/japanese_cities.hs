#!/usr/bin/env stack
-- stack --resolver lts-6.9 runghc --package http-conduit
-- Tool to find Japanese cities, their prefectures and locations from Wikidata
-- Only used to dump the list to be used in the actual fetcher, so a lot of
-- error checking is omitted.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client

import Data.Aeson
import Data.String
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

import Network.HTTP.Simple

import Text.Read (readMaybe)


data QID = Q { unQ :: Int }
    deriving (Eq, Ord)

instance Show QID where
    show (Q qid) = "Q" ++ show qid

instance Read QID where
    readsPrec p ('Q':qid) = case readsPrec p qid of
                              [(number, rest)] -> [(Q number, rest)]
                              _ -> []
    readsPrec _ _ = []

instance FromJSON QID where
    parseJSON = withText "QID" $
        \t -> let s = T.unpack t in case readMaybe s of
                  Just qid -> pure qid
                  Nothing -> fail $ "Not a QID: " ++ s

instance FromJSON v => FromJSON (M.Map QID v) where
    parseJSON v = M.mapKeys (fromJust . readMaybe) <$> parseJSON v

data CategoryResponse = CategoryResponse [QID]

data CategoryResponseItem = CategoryResponseItem { criQID :: QID }

instance FromJSON CategoryResponseItem where
    parseJSON = withObject "CategoryResponseItem" $ \o ->
        (CategoryResponseItem . unCRIURL) <$> (o .: "item" >>= (.: "value"))

unCRIURL :: String -> QID
unCRIURL = fromJust . readMaybe . reverse . takeWhile (/= '/') . reverse

instance FromJSON CategoryResponse where
    parseJSON = withObject "CategoryResponse" $ \o ->
        (CategoryResponse . map criQID) <$> (o .: "results" >>= (.: "bindings"))

inCategory :: QID -> IO [QID]
inCategory catid = do
    response <- httpJSON $ fromString $ "https://query.wikidata.org/sparql?query=SELECT%20?item%20WHERE%20{%20?item%20wdt:P31%20wd:" ++ show catid ++ "%20}&format=json"
    let CategoryResponse ids = getResponseBody response
    return ids

data WikidataResponse = WikidataResponse (M.Map QID WikidataEntity)
    deriving (Show)

data WikidataEntity = WikidataEntity { weName :: Text
                                     , weProperties :: M.Map Text [WikidataProperty]
                                     }
    deriving (Show)

-- Purposefully incomplete and lax typed
data WikidataProperty = WikidataProperty { wpValue :: Value }
    deriving (Show)

wpParse :: FromJSON a => WikidataProperty -> Result a
wpParse = fromJSON . wpValue

instance FromJSON WikidataResponse where
    parseJSON = withObject "WikidataResponse" $ \o ->
        WikidataResponse <$> o .: "entities"

instance FromJSON WikidataEntity where
    parseJSON = withObject "WikidataEntity" $ \o ->
        WikidataEntity <$> (o .: "labels" >>= (.: "en") >>= (.: "value"))
                       <*> o .: "claims"

instance FromJSON WikidataProperty where
    parseJSON = withObject "WikidataProperty" $ \o ->
        WikidataProperty <$> (o .: "mainsnak" >>= (.: "datavalue") >>= (.: "value"))

entity :: QID -> IO WikidataEntity
entity qid = do
    response <- httpJSON $ fromString $ "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=" ++ show qid ++ "&format=json"
    let (WikidataResponse entities) = getResponseBody response
    return $ entities ! qid

data Location = Location { lLon :: Float
                         , lLat :: Float
                         }
    deriving (Show)

instance FromJSON Location where
    parseJSON = withObject "Location" $ \o ->
        Location <$> o .: "longitude"
                 <*> o .: "latitude"

data LocatedIn = LocatedIn QID
    deriving (Show)

instance FromJSON LocatedIn where
    parseJSON = withObject "LocatedIn" $ \o ->
        (LocatedIn . Q) <$> o .: "numeric-id"

data City = City { cName :: Text
                 , cPrefectureName :: Text
                 , cLocation :: Location
                 }
    deriving (Show)

city :: QID -> IO City
city qid = do
    cityEntity <- entity qid
    let [Success location] = map wpParse $ weProperties cityEntity ! "P625"
    let [Success (LocatedIn prefectureID)] = map wpParse $ weProperties cityEntity ! "P131"
    prefectureEntity <- entity prefectureID
    return $ City (weName cityEntity) (weName prefectureEntity) location

governmentOrdnanceCities :: QID
governmentOrdnanceCities = Q 1749269

coreCities :: QID
coreCities = Q 1137833

specialCities :: QID
specialCities = Q 1145012

cities :: QID
cities = Q 494721

main :: IO ()
main = do
    gocs <- inCategory governmentOrdnanceCities
    cities <- mapM city gocs
    print cities
