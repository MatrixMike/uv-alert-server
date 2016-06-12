module Fetcher.Arpansa where

{- Fetch UV alert data from ARPANSA. -}

import Codec.Picture

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime

import Network.HTTP.Client

import Fetcher.Arpansa.Base
import Fetcher.Arpansa.CharacterRecognizer
import Fetcher.Base
import Fetcher.HTTP
import Types
import Types.Config
import Types.Location
import Utils


arpansaFetcher :: Fetcher
arpansaFetcher = Fetcher "ARPANSA" fetchArpansa (map fst addresses)

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

fetchArpansa :: AppM [Forecast]
fetchArpansa = do
    manager <- liftIO $ newManager defaultManagerSettings
    liftM concat $ forM addresses $ \(loc, address) -> do
        logStr $ "Fetching graph for " ++ loc ^. locCity ++ "..."
        logErrors address $ do
            graphBytes <- fetchHTTP manager address
            case decodeImage graphBytes of
                Left err -> do
                    logStr err
                    return []
                Right graphImage -> do
                    time <- liftIO getCurrentTime
                    let forecast = parseGraph loc graphImage time
                    return $ maybeToList forecast

maybeMinimum :: Ord a => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum xs = Just $ minimum xs

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs

parseGraph :: Location -> DynamicImage -> UTCTime -> Maybe Forecast
parseGraph loc image updated = do
    let uvLine = selectBestLine image
    let graph = map graphCoordinates uvLine
    let alertTimes = map fst $ filter (isDangerous . snd) graph
    astart <- maybeMinimum alertTimes
    aend <- maybeMaximum alertTimes
    date <- eitherToMaybe $ parseDate image
    let mlevel = maximum $ map snd graph
    return Forecast { _fcLocation = loc
                    , _fcDate = date
                    , _fcAlertStart = astart
                    , _fcAlertEnd = aend
                    , _fcMaxLevel = mlevel
                    , _fcUpdated = updated
                    }

forecastLineColor :: PixelRGB8
forecastLineColor = PixelRGB8 248 135 0

actualLineColor :: PixelRGB8
actualLineColor = PixelRGB8 153 255 255

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

graphLevel :: Int -> UVLevel
graphLevel = UVLevel . round . extrapolateLevel . realToFrac
    where extrapolateLevel :: Double -> Double
          extrapolateLevel = extrapolate (438, 0) (106, 16)

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
