module Fetcher.Arpansa where

{- Fetch UV alert data from ARPANSA. -}

import Codec.Picture

import Control.Arrow
import Control.Exception.Lifted
import Control.Lens
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
import Fetcher.Base
import Types
import Types.Location


arpansaFetcher :: Fetcher
arpansaFetcher = Fetcher "ARPANSA" fetchArpansa

addresses :: [(Location, String)]
addresses = map makeLocation [ (sa, "Adelaide", "adl")
                             , (nt, "Alice Springs", "ali")
                             , (qld, "Brisbane", "bri")
                             , (act, "Canberra", "can")
                             , (nt, "Darwin", "dar")
                             , (tas, "Kingston", "kin")
                             , (vic, "Melbourne", "mel")
                             , (nsw, "Newcastle", "new")
                             , (wa, "Perth", "per")
                             , (nsw, "Sydney", "syd")
                             , (qld, "Townsville", "tow")
                             ]
    where makeLocation (state, town, abbr) = (Location "Australia" state town, cityAddress abbr)
          cityAddress abbr = "http://www.arpansa.gov.au/uvindex/realtime/images/" ++ abbr ++ "_rt.gif"
          act = "Australian Capital Territory"
          nsw = "New South Wales"
          nt = "Northern Territory"
          qld = "Queensland"
          sa = "South Australia"
          tas = "Tasmania"
          vic = "Victoria"
          wa = "Western Australia"

dateIn :: Location -> IO Day
dateIn loc = do
    utcTime <- getCurrentTime
    let tz = locTZ loc
    let cityTime = utcToLocalTime (timeZoneFromSeries tz utcTime) utcTime
    return $ localDay cityTime

fetchArpansa :: AppM [Forecast]
fetchArpansa = do
    manager <- liftIO $ newManager defaultManagerSettings
    liftM concat $ forM addresses $ \(loc, address) -> do
        logStr $ "Fetching graph for " ++ loc ^. locCity ++ "..."
        today <- liftIO $ dateIn loc
        handle (logError address) $ do
            graphBytes <- fetchGraph manager address
            case decodeImage graphBytes of
                Left err -> do
                    logStr err
                    return []
                Right graphImage -> do
                    time <- liftIO getCurrentTime
                    let forecast = parseGraph loc today graphImage time
                    return $ maybeToList forecast

fetchGraph :: Manager -> String -> AppM BS.ByteString
fetchGraph manager address = do
    request <- parseUrl address
    chunks <- liftIO $ withResponse request manager (brConsume . responseBody)
    return $ BS.concat chunks

maybeMinimum :: Ord a => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum xs = Just $ minimum xs

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs

parseGraph :: Location -> Day -> DynamicImage -> UTCTime -> Maybe Forecast
parseGraph loc day image updated = do
    let uvLine = selectBestLine image
    let graph = map graphCoordinates uvLine
    let alertTimes = map fst $ filter ((>= alertLevel) . snd) graph
    astart <- maybeMinimum alertTimes
    aend <- maybeMaximum alertTimes
    let mlevel = maximum $ map snd graph
    return Forecast { _fcLocation = loc
                    -- TODO Can read this from the image
                    , _fcDate = day
                    , _fcAlertStart = astart
                    , _fcAlertEnd = aend
                    , _fcMaxLevel = mlevel
                    , _fcUpdated = updated
                    }

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

charAt :: ImageCoord -> DynamicImage -> Char
charAt = undefined
