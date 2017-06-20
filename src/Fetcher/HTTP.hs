module Fetcher.HTTP (
    fetchHTTP,
) where

{- Common functions for fetchers using HTTP -}

import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLBS, parseRequest)

import Types.Config


fetchHTTP :: String -> AppM BS.ByteString
fetchHTTP address = do
    request <- parseRequest address
    chunks <- liftIO $ responseBody <$> httpLBS request
    return $ LBS.toStrict chunks
