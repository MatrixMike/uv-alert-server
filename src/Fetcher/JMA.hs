module Fetcher.JMA where

{- Fetch UV alert data from Japan Meteorological Agency. -}

import Codec.Picture

import qualified Data.Map as M
import Data.Time
import Data.Time.LocalTime.TimeZone.Series

import Fetcher.Base
import Types
import Types.Config
import Types.Location
import Types.Location.Japan


jmaFetcher :: Fetcher
jmaFetcher = Fetcher "JMA" fetchJma (map fst cities)

-- TODO: Use real city coordinates
cities :: [(Location, (Int, Int))]
cities = [ (loc "Tokyo" "Tokyo", (324, 212))
         ]
             where loc = Location "Japan"

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
fetchJma = undefined

imageName :: UTCTime -> Int -> String
imageName now index = urlBase ++
    zeroPad 4 year ++ zeroPad 2 month ++ zeroPad 2 day ++
    zeroPad 2 fcHour ++ "00-" ++ zeroPad 2 index ++ ".png"
        where zeroPad n val = take (n - length str) (repeat '0') ++ str
                  where str = show val
              LocalTime date time = utcToLocalTime' japanTZ now
              TimeOfDay hour _ _ = time
              (fcDate, fcHour) = if hour < 6 then (addDays (-1) date, 18)
                                             else if hour < 18 then (date, 6)
                                             else (date, 18)
              (year, month, day) = toGregorian fcDate
              urlBase = "http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/"

imageUVLevel :: Int -> Int -> DynamicImage -> Maybe UVLevel
imageUVLevel x y (ImageRGB8 image) = M.lookup (pixelAt image x y) levels
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
