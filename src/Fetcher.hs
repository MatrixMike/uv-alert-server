module Fetcher where

import Control.Concurrent

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Either

import Network.FTP.Client
import Network.URI

import App
import Data
import Pusher


updateInterval :: Int
updateInterval = 3600000 -- milliseconds

-- One of these is for today, another one for tomorrow
address :: URI
Just address = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP007.txt"

address2 :: URI
Just address2 = parseURI "ftp://ftp2.bom.gov.au/anon/gen/fwo/IDYGP026.txt"


runFetcher :: Config -> IO ()
runFetcher cfg = runReaderT fetcher cfg

fetcher :: AppM ()
fetcher = forever $ do
    fetch
    liftIO $ threadDelay updateInterval

fetch :: AppM ()
fetch = do
    content <- fetchLines address
    let forecasts = rights $ map parseForecast $ lines content
    stateM $ modify $
        \store -> store { forecasts = forecasts }
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
