module Fetcher.Arpansa where

{- Fetch UV alert data from ARPANSA. -}

import Codec.Picture

import Control.Arrow
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import Data.Maybe
import Data.String
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series

import Network.HTTP.Client
import Network.URI

import App
import Data
import Fetcher.Base
import TZ


arpansaFetcher :: Fetcher
arpansaFetcher = Fetcher "ARPANSA" fetchArpansa

-- TODO: find other abbreviations on http://www.arpansa.gov.au/uvindex/realtime/
addresses :: [(String, String)]
addresses = map (second cityAddress) [ ("Adelaide", "adl")
                                     , ("Alice Springs", "ali")
                                     , ("Brisbane", "bri")
                                     , ("Canberra", "can")
                                     , ("Darwin", "dar")
                                     , ("Kingston", "kin")
                                     , ("Melbourne", "mel")
                                     , ("Newcastle", "new")
                                     , ("Perth", "per")
                                     , ("Sydney", "syd")
                                     , ("Townsville", "tow")
                                     ]
    where cityAddress abbr = "http://www.arpansa.gov.au/uvindex/realtime/images/" ++ abbr ++ "_rt.gif"

dateInCity :: String -> IO Day
dateInCity city = do
    utcTime <- getCurrentTime
    let tz = cityTZ city
    let cityTime = utcToLocalTime (timeZoneFromSeries tz utcTime) utcTime
    return $ localDay cityTime

fetchArpansa :: AppM [Forecast]
fetchArpansa = do
    manager <- liftIO $ newManager defaultManagerSettings
    liftM concat $ forM addresses $ \(city, address) -> do
        logStr $ "Fetching graph for " ++ city ++ "..."
        today <- liftIO $ dateInCity city
        handle (logError address) $ do
            graphBytes <- fetchGraph manager address
            case decodeImage graphBytes of
                Left err -> do
                    logStr err
                    return []
                Right graphImage -> do
                    time <- liftIO getCurrentTime
                    let forecast = parseGraph city today graphImage time
                    return [forecast]

fetchGraph :: Manager -> String -> AppM BS.ByteString
fetchGraph manager address = do
    request <- parseUrl address
    chunks <- liftIO $ withResponse request manager (brConsume . responseBody)
    return $ BS.concat chunks

parseGraph :: String -> Day -> DynamicImage -> UTCTime -> Forecast
parseGraph city day image updated = Forecast { _fcLocation = Location { _locCity = city }
                                             -- TODO Can read this from the image
                                             , _fcDate = day
                                             , _fcAlertStart = astart
                                             , _fcAlertEnd = aend
                                             , _fcMaxLevel = mlevel
                                             , _fcUpdated = updated
                                             }
    where uvLine = selectBestLine image
          graph = map graphCoordinates uvLine
          alertTimes = map fst $ filter ((>= alertLevel) . snd) graph
          -- TODO: no alert if mlevel is low all the time
          astart = minimum alertTimes
          aend = maximum alertTimes
          mlevel = maximum $ map snd graph

forecastLineColor :: PixelRGB8
forecastLineColor = PixelRGB8 248 135 0

actualLineColor :: PixelRGB8
actualLineColor = PixelRGB8 153 255 255

type ImageCoord = (Int, Int)

selectPixels :: PixelRGB8 -> DynamicImage -> [ImageCoord]
selectPixels color (ImageRGB8 image) = filter colorMatches indices
    where colorMatches (x, y) = pixelAt image x y == color
          indices = [(x, y) | x <- [0..imageWidth image - 1]
                            , y <- [0..imageHeight image - 1]]
selectPixels _ _ = []  -- TODO: Support image types generically?

isLegend :: ImageCoord -> Bool
isLegend (x, y) = x > 740 || y > 460

selectForecastLine :: DynamicImage -> [ImageCoord]
selectForecastLine = filter (not . isLegend) . selectPixels forecastLineColor

selectActualLine :: DynamicImage -> [ImageCoord]
selectActualLine = filter (not . isLegend) . selectPixels actualLineColor

selectBestLine :: DynamicImage -> [ImageCoord]
selectBestLine img = filter (\(x, _) -> x > actualEnd) forecastLine ++ actualLine
    where forecastLine = selectForecastLine img
          actualLine = selectActualLine img
          -- take a low value in case no actual line is drawn yet
          actualEnd = maximum $ map fst actualLine ++ [0]

extrapolate :: Fractional a => (a, a) -> (a, a) -> a -> a
extrapolate (a1, b1) (a2, b2) a = b1 + (b2 - b1) * (a - a1) / (a2 - a1)

graphLevel :: Int -> UVLevel
graphLevel = UVLevel . round . extrapolate (438, 0) (106, 16) . realToFrac

graphTimeOfDay :: Int -> TimeOfDay
graphTimeOfDay = floatToTod . extrapolate (83, t6) (723, t20) . realToFrac
    -- TODO: use picosecondsToDiffTime and diffTimeToPicoseconds from time 1.6
    where t6 :: Float
          t6 = todToFloat $ TimeOfDay 6 0 0
          t20 :: Float
          t20 = todToFloat $ TimeOfDay 20 0 0
          todToFloat :: TimeOfDay -> Float
          todToFloat = realToFrac . timeOfDayToTime
          floatToTod :: Float -> TimeOfDay
          floatToTod = timeToTimeOfDay . realToFrac

type GraphCoord = (TimeOfDay, UVLevel)

graphCoordinates :: ImageCoord -> GraphCoord
graphCoordinates = first graphTimeOfDay . second graphLevel
