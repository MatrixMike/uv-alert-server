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
      let inBrackets =
            T.drop 1 . T.dropEnd 1 . T.dropWhile (/= '(') $
            T.dropWhileEnd (/= ')') valueStr
      let [lonStr, latStr] = T.splitOn " " inBrackets
      let lLon = fromJust $ readMaybe $ T.unpack lonStr
      let lLat = fromJust $ readMaybe $ T.unpack latStr
      pure $ SparqlValue Location {..}

data City = City
  { cName :: Text
  , cRegionName :: Text
  , cLocation :: Location
  , cTimeZone :: Text
  , cWikipediaName :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON (SparqlValue City) where
  parseJSON =
    withObject "city" $ \o -> do
      cName <- unSparqlValue <$> o .: "city"
      cRegionName <- unSparqlValue <$> o .: "region"
      cLocation <- unSparqlValue <$> o .: "location"
      tz <- fmap unSparqlValue <$> o .:? "timezone"
      let cTimeZone = fromMaybe "" tz
      cWikipediaName <-
        (T.takeWhileEnd (/= '/') . unSparqlValue) <$> o .: "article"
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
usCities limit = do
  cities <-
    nubOrd . map unSparqlValue <$>
    wikidata
      [i|
      SELECT ?item ?state ?city ?region ?location ?timezone ?article
      WHERE {
        ?item wdt:P31 wd:Q515. # is-a city
        ?item wdt:P17 wd:Q30. # country USA
        ?item wdt:P131+ ?state. # located, transitively, in
        ?state wdt:P31 wd:Q35657. # is-a USA state
        ?item wdt:P1082 ?population. # population
        ?item rdfs:label ?city FILTER (lang(?city) = 'en').
        ?state rdfs:label ?region FILTER (lang(?region) = 'en').
        ?item wdt:P625 ?location. # coordinate location
        OPTIONAL {
          ?item wdt:P421 ?tz. # located in time zone
          ?tz wdt:P31 wd:Q12143. # is-a time zone
          ?tz rdfs:label ?timezone FILTER (lang(?timezone) = 'en').
        }
        OPTIONAL {
          ?article schema:about ?item.
          ?article schema:isPartOf <https://en.wikipedia.org/> .
        }
      }
      ORDER BY desc(?population)
      LIMIT ${limit}
    |]
  traverse backfillTimezone cities

timezoneFromWPArticle :: Text -> Either String Text
timezoneFromWPArticle text = do
  let tzLines =
        filter (T.isPrefixOf "|timezone") $ T.lines $ T.replace "| " "|" text
  tzLine <-
    case tzLines of
      (tzl:_) -> pure tzl
      _ -> error [i|No timezone line in ${text}|]
  let tz =
        T.takeWhile (/= '|') $
        T.dropWhileEnd (== ']') $
        T.dropWhile (== '[') $ T.dropWhile (/= '[') tzLine
  pure tz

backfillTimezone :: City -> IO City
backfillTimezone city
  | cTimeZone city /= "" = pure city
  | otherwise = do
    let request =
          fromString
            [i|https://en.wikipedia.org/w/api.php?action=query&titles=${T.unpack $ cWikipediaName city}&prop=revisions&rvprop=content&format=json|]
    response <- httpJSON request
    let text' =
          flip A.parseEither (getResponseBody response) $
          withObject "wikipedia response" $ \o -> do
            query <- o .: "query"
            pages <- query .: "pages"
            pageId <-
              case HM.keys pages of
                [p] -> pure p
                _ -> fail "no single page id"
            page <- pages .: pageId
            revisions <- page .: "revisions"
            revision <-
              case revisions of
                (r:_) -> pure r
                _ -> fail "no revision"
            result <- revision .: "*"
            pure result
    text <-
      case text' of
        Left err -> error err
        Right t -> pure t
    tz <-
      case timezoneFromWPArticle text of
        Left err -> error err
        Right tz' -> pure tz'
    pure $ city {cTimeZone = tz}

japanCities :: Int -> IO [City]
japanCities limit =
  nubOrd . map unSparqlValue <$>
  wikidata
    [i|
      SELECT ?item ?state ?city ?region ?location ?timezone ?article
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
        ?item wdt:P625 ?location. # coordinate location
        VALUES ?tz { wd:Q909085 } # Japan Standard Time
        ?tz rdfs:label ?timezone FILTER (lang(?timezone) = 'en').
        OPTIONAL {
          ?article schema:about ?item.
          ?article schema:isPartOf <https://en.wikipedia.org/> .
        }
      }
      ORDER BY desc(?population)
      LIMIT ${limit}
  |]

cityRepr :: City -> Text
cityRepr city_ = [i|, loc "${region}" "${name}" (latlon ${lat} ${lon}) ${tz}|]
  where
    name = fixName $ cName city_
    fixName = T.replace " City" ""
    region = fixRegion $ cRegionName city_
    fixRegion = T.replace " Prefecture" "" . T.replace " Subprefecture" ""
    lat = lLat $ cLocation city_
    lon = lLon $ cLocation city_
    tz :: Text
    tz = tzVar $ cTimeZone city_

tzVar :: Text -> Text
tzVar =
  go .
  remove "North American " .
  remove " (North America)" .
  remove " (Americas)" .
  remove " Standard" . remove " Daylight" . remove " Time" . remove " Zone"
  where
    remove s = T.replace s ""
    go "Japan" = "japan"
    go "Alaska" = "usAlaska"
    go "Eastern" = "usEastern"
    go "Pacific" = "usPacific"
    go "Central" = "usCentral"
    go "Mountain" = "usMountain"
    go "UTC−04:00" = "usCentral"
    go "UTC−05:00" = "usEastern"
    go "UTC−06:00" = "usCentral"
    go "UTC−07:00" = "usMountain"
    go "UTC−08:00" = "usPacific"
    go unknown = [i|Unknown timezone "${unknown}".|]

main :: IO ()
main = do
  catKind <- getArgs
  let cityQuery =
        case catKind of
          ["japan"] -> japanCities 200
          ["us"] -> usCities 400
          _ -> error "Invalid category"
  cities <- cityQuery
  forM_ cities $ T.putStrLn . cityRepr
