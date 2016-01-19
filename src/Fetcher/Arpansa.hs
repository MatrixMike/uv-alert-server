module Fetcher.Arpansa where

{- Fetch UV alert data from ARPANSA. -}

import Codec.Picture

import Control.Arrow
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.String
import qualified Data.ByteString as BS

import Network.HTTP.Client
import Network.URI

import App
import Data
import Fetcher.Base


-- TODO: find other abbreviations on http://www.arpansa.gov.au/uvindex/realtime/
addresses :: [(String, String)]
addresses = map (second cityAddress) [ ("Melbourne", "mel")
                                     ]
    where cityAddress abbr = "http://www.arpansa.gov.au/uvindex/realtime/images/" ++ abbr ++ "_rt.gif"

fetchArpansa :: AppM [Forecast]
fetchArpansa = do
    manager <- liftIO $ newManager defaultManagerSettings
    liftM concat $ forM addresses $ \(city, address) -> do
        logStr $ "Fetching graph for " ++ city ++ "..."
        handle (logError address) $ do
            graphBytes <- fetchGraph manager address
            case decodeImage graphBytes of
                Left err -> do
                    logStr err
                    return []
                Right graphImage -> do
                    let forecast = parseGraph city graphImage
                    return [forecast]

fetchGraph :: Manager -> String -> AppM BS.ByteString
fetchGraph manager address = do
    request <- parseUrl address
    chunks <- liftIO $ withResponse request manager (brConsume . responseBody)
    return $ BS.concat chunks

parseGraph :: String -> DynamicImage -> Forecast
parseGraph = undefined

arpansaFetcher :: Fetcher
arpansaFetcher = Fetcher "ARPANSA" fetchArpansa
