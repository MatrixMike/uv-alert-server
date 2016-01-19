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

import Network.HTTP.Client
import Network.URI

import App
import Data
import Fetcher.Base


arpansaFetcher :: Fetcher
arpansaFetcher = Fetcher "ARPANSA" fetchArpansa

-- TODO: find other abbreviations on http://www.arpansa.gov.au/uvindex/realtime/
addresses :: [(String, String)]
addresses = map (second cityAddress) [ ("Melbourne", "mel")
                                     ]
    where cityAddress abbr = "http://www.arpansa.gov.au/uvindex/realtime/images/" ++ abbr ++ "_rt.gif"

fetchArpansa :: AppM [Forecast]
fetchArpansa = do
    manager <- liftIO $ newManager defaultManagerSettings
    today <- liftM utctDay $ liftIO getCurrentTime
    liftM concat $ forM addresses $ \(city, address) -> do
        logStr $ "Fetching graph for " ++ city ++ "..."
        handle (logError address) $ do
            graphBytes <- fetchGraph manager address
            case decodeImage graphBytes of
                Left err -> do
                    logStr err
                    return []
                Right graphImage -> do
                    let forecast = parseGraph city today graphImage
                    return [forecast]

fetchGraph :: Manager -> String -> AppM BS.ByteString
fetchGraph manager address = do
    request <- parseUrl address
    chunks <- liftIO $ withResponse request manager (brConsume . responseBody)
    return $ BS.concat chunks

parseGraph :: String -> Day -> DynamicImage -> Forecast
parseGraph city day image = Forecast { location = Location { city = city }
                                     -- TODO Can read this from the image
                                     , date = day
                                     , alertStart = astart
                                     , alertEnd = aend
                                     , maxLevel = mlevel
                                     }
    where uvLine = selectPixels forecastLineColor image
          astart = undefined
          aend = undefined
          mlevel = undefined

forecastLineColor :: PixelRGB8
forecastLineColor = PixelRGB8 248 135 0

selectPixels :: PixelRGB8 -> DynamicImage -> [(Int, Int)]
selectPixels color (ImageRGB8 image) = filter colorMatches indices
    where colorMatches (x, y) = pixelAt image x y == color
          indices = [(x, y) | x <- [0..imageWidth image - 1]
                            , y <- [0..imageHeight image - 1]]
selectPixels _ _ = []  -- TODO: Support image types generically?
