module Fetcher.BOM where

{-
Fetch UV forecast from Buerau of Meteorology.

Unfortunately, this data is free for personal use but not for redistribution.
-}

import Control.Concurrent

import Control.Exception.Lifted

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Either
import qualified Data.Set as S
import Data.Time.Clock

import Network.FTP.Client
import Network.URI

import App
import Data
import Fetcher.Base


-- Data for today
todayAddress :: URI
Just todayAddress = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP007.txt"

-- Data for tomorrow (forecast)
forecastAddress :: URI
Just forecastAddress = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP026.txt"

fetchBOM :: URI -> AppM [Forecast]
fetchBOM address = do
    logStr $ "Fetching " ++ show address ++ "..."
    handle (logError address) $ do
        content <- fetchLines address
        time <- liftIO getCurrentTime
        return $ rights $ map (parseForecast time) $ lines content

fetchLines :: MonadIO m => URI -> m String
fetchLines uri = liftIO $ do
    let (Just host) = liftM uriRegName $ uriAuthority uri
    conn <- easyConnectFTP host
    loginAnon conn
    (content, _) <- getbinary conn $ uriPath uri
    return content

fetchTestContent :: MonadIO m => m String
fetchTestContent = liftIO $ readFile "src/IDYGP007.txt"

bomFetchers = [ Fetcher "BOM today" (fetchBOM todayAddress)
              , Fetcher "BOM forecast" (fetchBOM forecastAddress)
              ]
