module Fetcher where

import Control.Concurrent

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Either
import Data.Time.Clock

import Network.FTP.Client
import Network.URI

import App
import Data
import Pusher


updateInterval :: Int -- microseconds
updateInterval = 3600 * 1000000

-- Data for today
todayAddress :: URI
Just todayAddress = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP007.txt"

-- Data for tomorrow (forecast)
forecastAddress :: URI
Just forecastAddress = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP026.txt"


runFetcher :: Config -> IO ()
runFetcher = runReaderT fetcher

fetcher :: AppM ()
fetcher = forever $ do
    fetch
    removeOld
    liftIO $ threadDelay updateInterval

fetch :: AppM ()
fetch = do
    forM_ [todayAddress, forecastAddress] $ \address -> do
        logStr $ "Fetching " ++ show address
        content <- fetchLines address
        let newForecasts = rights $ map parseForecast $ lines content
        stateM $ modify $
            \store -> store { forecasts = newForecasts ++ forecasts store }
    push

fetchLines :: MonadIO m => URI -> m String
fetchLines uri = liftIO $ do
    let (Just host) = liftM uriRegName $ uriAuthority uri
    conn <- easyConnectFTP host
    loginAnon conn
    (content, _) <- getbinary conn $ uriPath uri
    return content

fetchTestContent :: MonadIO m => m String
fetchTestContent = liftIO $ readFile "src/IDYGP007.txt"

removeOld :: AppM ()
removeOld = do
    now <- liftIO getCurrentTime
    stateM $ modify $
        \store -> store { forecasts = filter (isRecent now) $ forecasts store }
