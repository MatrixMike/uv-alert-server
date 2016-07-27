module Fetcher.JMA where

{- Fetch UV alert data from Japan Meteorological Agency. -}

import Codec.Picture
import Codec.Picture.Types

import Control.Monad
import Control.Monad.IO.Class

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Time.LocalTime.TimeZone.Series

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Fetcher.Base
import Fetcher.HTTP
import Fetcher.JMA.Cities
import Types
import Types.Config
import Types.Location
import Types.Location.Japan


jmaFetcher :: Fetcher
jmaFetcher = Fetcher "JMA" fetchJma (map fst cities)

{-
JMA UV index page: http://www.jma.go.jp/en/uv/

JMA provides the following images for the next day UV forecast:

http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/201607201800-00.png
...
http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/201607201800-12.png

File name format is the time forecast was made (18:00 the previous day) plus
the index of the forecast hour (00 = 06:00 the next day, 12 = 18:00).

Forecast is updated at 18:00 previous day and 06:00 on the day; the
latter has the following files:

http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/201607210600-00.png
...
http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/201607210600-12.png

The index is the forecast hour (00 = 06:00 the same day, 12 = 18:00).

After 18:00, the following images contain data for the current day:

http://www.jma.go.jp/en/uv/imgs/uv_color/analysis/000/201607201800-02.png
...
http://www.jma.go.jp/en/uv/imgs/uv_color/analysis/000/201607201800-10.png

File name format is the time data was updated (as specified in the image
itself) plus the index of the hour (02 = 08:00, 10 = 16:00).

All times are in JST, the single Japan time zone (there is no daylight saving
time).

TODO: Is live data limited to 08:00-16:00, or does this depend on the UV level?
-}

fetchJma :: AppM [Forecast]
fetchJma = do
    manager <- liftIO $ newManager tlsManagerSettings
    time <- liftIO getCurrentTime
    images <- forM (map (imageNameTime time) imageRange) $
        \(address, imgTime) -> logErrors address $ do
             logStr $ "Fetching JMA forecast for " ++ show (utcToLocalTime' japanTZ imgTime) ++ "..."
             imgBytes <- fetchHTTP manager address
             case decodeImage imgBytes of
               Left err -> do
                   logStr err
                   return Nothing
               Right img -> return $ Just (img, imgTime)
    case sequence images of
      Nothing -> return []
      Just images' -> do
          return $ catMaybes $ map (forecast time images') cities

imageRange :: [Int]
imageRange = [0..12]

imageNameTime :: UTCTime -> Int -> (String, UTCTime)
imageNameTime now index = (url, fcTime)
    where url = urlBase ++ zeroPad 4 year ++ zp2 month ++ zp2 day ++
                zp2 fcUpdatedHour ++ "00-" ++ zp2 index ++ ".png"
          zeroPad n val = take (n - length str) (repeat '0') ++ str
              where str = show val
          zp2 = zeroPad 2
          LocalTime date time = utcToLocalTime' japanTZ now
          TimeOfDay hour _ _ = time
          (fcUpdatedDate, fcUpdatedHour) = if hour < 6 then (addDays (-1) date, 18)
                                                       else if hour < 18 then (date, 6)
                                                       else (date, 18)
          (year, month, day) = toGregorian fcUpdatedDate
          fcEffectiveDate = if hour < 18 then date else addDays 1 date
          fcEffectiveHour = index + 6
          fcTime = localTimeToUTC' japanTZ $ LocalTime fcEffectiveDate $ TimeOfDay fcEffectiveHour 0 0
          urlBase = "http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/"

imageUVLevelExact :: ImageCoord -> DynamicImage -> Maybe UVLevel
imageUVLevelExact (ImageCoord x y) (ImageRGB8 image) = M.lookup (pixelAt image x y) levels
    where levels = M.fromList $ zip levelColors $ map UVLevel [0..]
          levelColors = [ PixelRGB8 255 255 255
                        , PixelRGB8 217 217 255
                        , PixelRGB8 153 203 255
                        , PixelRGB8 255 255 190
                        , PixelRGB8 250 250 150
                        , PixelRGB8 250 245   0
                        , PixelRGB8 255 200   0
                        , PixelRGB8 255 140   0
                        , PixelRGB8 250  90   0
                        , PixelRGB8 255  20   0
                        , PixelRGB8 165   0  33
                        , PixelRGB8 181   0  91
                        , PixelRGB8 204   0 160
                        , PixelRGB8 204   0 204
                        ]

maxDist :: Int
maxDist = 10

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

-- Average the UV levels, if there's at least one present
averageLevel :: [Maybe UVLevel] -> Maybe UVLevel
averageLevel lvls = case catMaybes lvls of
                      [] -> Nothing
                      lvls' -> Just $ UVLevel $ round $ fromIntegral (sum (map _uvValue lvls')) / fromIntegral (length lvls')

-- Some pixels on the map are always black (shorelines). Find the closest pixel
-- that isn't and return its UV level.
-- Don't stray too far
imageUVLevel :: ImageCoord -> DynamicImage -> Maybe UVLevel
imageUVLevel coo img = firstJust $ map averageLevel levels
    where circles :: [[ImageCoord]]
          circles = map (circleAround coo img) [0..maxDist]
          levels :: [[Maybe UVLevel]]
          levels = map (map (flip imageUVLevelExact img)) circles

-- All points at most dist pixels away from the given point, sorted by distance
-- to that point
circleAround :: ImageCoord -> DynamicImage -> Int -> [ImageCoord]
circleAround coo img dist = sortOn (distance coo) $ filter ((< (fromIntegral dist)) . distance coo) square
    where square = [ImageCoord x y
                   | x <- around (icX coo) dist
                   , y <- around (icY coo) dist
                   , x >= 0
                   , y >= 0
                   , x < dynamicMap imageWidth img
                   , y < dynamicMap imageHeight img
                   ]

around val spread = [val - spread .. val + spread]

distance :: ImageCoord -> ImageCoord -> Float
distance c1 c2 = sqrt (dx * dx + dy * dy)
    where dx = fromIntegral $ icX c1 - icX c2
          dy = fromIntegral $ icY c1 - icY c2

imageUVLevels :: ImageCoord -> [DynamicImage] -> Maybe [UVLevel]
imageUVLevels coo = traverse (imageUVLevel coo)

forecast :: UTCTime -> [(DynamicImage, UTCTime)] -> (Location, LonLat) -> Maybe Forecast
forecast time imagesTimes (loc, lonlat) = buildForecast loc time =<< measurements
    where (images, times) = unzip imagesTimes
          measurements :: Maybe [(UTCTime, UVLevel)]
          measurements = zip times <$> (imageUVLevels coo images)
          coo = imageCoord lonlat
