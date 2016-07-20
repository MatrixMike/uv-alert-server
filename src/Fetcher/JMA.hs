module Fetcher.JMA where

{- Fetch UV alert data from Japan Meteorological Agency. -}

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

Forecast seems to be updated at 18:00 previous day and 06:00 on the day; the
latter has the following files:

http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/201607210600-00.png
...
http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/201607210600-12.png

The following images contain data for the current day:

http://www.jma.go.jp/en/uv/imgs/uv_color/analysis/000/201607201800-02.png
...
http://www.jma.go.jp/en/uv/imgs/uv_color/analysis/000/201607201800-10.png

File name format is the time data was updated (as specified in the image
itself; TODO: does it update during the day?) plus the index of the hour
(02 = 08:00, 10 = 16:00).

All times are in JST, the single Japan time zone (there is no daylight saving
time).

TODO: What is the live data address during the day?

TODO: Is live data limited to 08:00-16:00, or does this depend on the UV level?
-}

fetchJma :: AppM [Forecast]
fetchJma = undefined
