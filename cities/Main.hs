{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Tool to find cities, their prefectures/states and locations from Wikidata
Only used to dump the list to be used in the actual fetcher, so a lot of
error checking is omitted.
-}
import Control.Monad
import Control.Retry

import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.List.Extra
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.String.Here
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

import Network.HTTP.Simple hiding (Proxy)
import Network.URI.Encode as URIEncode

import System.Environment

import Text.Read (readMaybe)

newtype QID = Q
  { unQ :: Int
  } deriving (Eq, Ord)

instance Show QID where
  show qid = "Q" ++ show (unQ qid)

instance Read QID where
  readsPrec p ('Q':qid) =
    case readsPrec p qid of
      [(number, rest)] -> [(Q number, rest)]
      _ -> []
  readsPrec _ _ = []

newtype SparqlResponse a = SparqlResponse
  { unSparqlResponse :: [a]
  }

instance FromJSON a => FromJSON (SparqlResponse a) where
  parseJSON =
    withObject "SPARQL response" $ \o ->
      SparqlResponse <$> (o .: "results" >>= (.: "bindings"))

newtype SparqlValue a = SparqlValue
  { unSparqlValue :: a
  }

instance FromJSON (SparqlValue Text) where
  parseJSON = withObject "SPARQL text" $ \o -> SparqlValue <$> o .: "value"

instance FromJSON (SparqlValue QID) where
  parseJSON =
    withObject "SPARQL QID" $ \o ->
      (SparqlValue . unWikiDataURL) <$> o .: "value"

unWikiDataURL :: Text -> QID
unWikiDataURL = fromJust . readMaybe . T.unpack . T.takeWhileEnd (/= '/')

myRetry :: IO a -> IO a
myRetry act = recoverAll def $ const act

wikidata :: FromJSON a => String -> IO [a]
wikidata sparql =
  myRetry $ do
    let request =
          fromString
            [i|https://query.wikidata.org/sparql?query=${URIEncode.encode sparql}&format=json|]
    -- r1 <- httpLBS request
    -- T.putStrLn $ T.decodeUtf8 $ LBS.toStrict $ getResponseBody r1
    response <- httpJSON request
    return $ unSparqlResponse $ getResponseBody response

data Location = Location
  { lLon :: Float
  , lLat :: Float
  } deriving (Eq, Ord, Show)

instance FromJSON (SparqlValue Location) where
  parseJSON =
    withObject "Location" $ \o -> do
      valueStr <- o .: "value"
      let inBrackets = T.drop 1 . T.dropEnd 1 . T.dropWhile (/= '(') $ T.dropWhileEnd (/= ')') valueStr
      let [lonStr, latStr] = T.splitOn " " inBrackets
      let lLon = fromJust $ readMaybe $ T.unpack lonStr
      let lLat = fromJust $ readMaybe $ T.unpack latStr
      pure $ SparqlValue Location {..}

data City = City
  { cName :: Text
  , cRegionName :: Text
  , cLocation :: Location
  } deriving (Eq, Ord, Show)

instance FromJSON (SparqlValue City) where
  parseJSON = withObject "city" $ \o -> do
    cName <- unSparqlValue <$> o .: "city"
    cRegionName <- unSparqlValue <$> o .: "region"
    cLocation <- unSparqlValue <$> o .: "location"
    pure $ SparqlValue City {..}

instance FromJSON (SparqlValue (M.Map Text Text)) where
  parseJSON =
    withObject "map" $ \o ->
      (SparqlValue . M.fromList) <$> traverse parseItem (HM.toList o)
    where
      parseItem :: (Text, Value) -> A.Parser (Text, Text)
      parseItem (k, v) = do
        v' <- unSparqlValue <$> parseJSON v
        pure (k, v')

usCities :: Int -> IO [City]
usCities limit =
  nubOrd . map unSparqlValue <$> wikidata
    [i|
      SELECT ?item ?state ?city ?region ?location
      WHERE {
        ?item wdt:P31 wd:Q515. # is-a city
        ?item wdt:P17 wd:Q30. # country USA
        ?item wdt:P131+ ?state. # located, transitively, in
        ?state wdt:P31 wd:Q35657. # is-a USA state
        ?item wdt:P1082 ?population. # population
        ?item rdfs:label ?city FILTER (lang(?city) = 'en').
        ?state rdfs:label ?region FILTER (lang(?region) = 'en').
        ?item wdt:P625 ?location # coordinate location
      }
      ORDER BY desc(?population)
      LIMIT ${limit}
  |]

japanCities :: Int -> IO [City]
japanCities limit =
  nubOrd . map unSparqlValue <$> wikidata
    [i|
      SELECT ?item ?state ?city ?region ?location
      WHERE {
        {
          {
            ?item wdt:P31 wd:Q1749269. # is-a 政令指定都市
          } UNION {
            ?item wdt:P31 wd:Q1137833. # is-a 中核市
          } UNION {
            ?item wdt:P31 wd:Q1145012 # is-a 特例市
          } UNION {
            ?item wdt:P31 wd:Q494721 # is-a Japanese city
          }
          ?item wdt:P131 ?prefecture. # located, transitively, in
          ?prefecture wdt:P31 wd:Q50337. # is-a Japanese prefecture
        }
        UNION
        {
          # Tokyo
          ?item wdt:P1376 wd:Q17. # capital of Japan
          ?prefecture wdt:P1376 wd:Q17 FILTER(?item = ?prefecture). # its own prefecture
        }
        ?item wdt:P1082 ?population. # population
        ?item rdfs:label ?city FILTER (lang(?city) = 'en').
        ?prefecture rdfs:label ?region FILTER (lang(?region) = 'en').
        ?item wdt:P625 ?location # coordinate location
      }
      ORDER BY desc(?population)
      LIMIT ${limit}
  |]

cityRepr :: City -> Text
cityRepr city_ =
  [i|, loc "${region}" "${name}" \$ latlon ${lat} ${lon}|]
  where
    name = fixName $ cName city_
    region = fixRegion $ cRegionName city_
    lat = lLat $ cLocation city_
    lon = lLon $ cLocation city_
    fixName = T.replace " City" ""
    fixRegion = T.replace " Prefecture" "" . T.replace " Subprefecture" ""

-- cities :: QID
-- cities = Q 494721

main :: IO ()
main = do
  catKind <- getArgs
  let cityQuery =
        case catKind of
          ["japan"] -> japanCities 200
          ["us"] -> usCities 400
          _ -> error "Invalid category"
  cities_ <- cityQuery
  forM_ cities_ $ T.putStrLn . cityRepr
