module Fetcher.HTTP (
    fetchHTTP,
) where

{- Common functions for fetchers using HTTP -}

import Control.Monad.IO.Class

import qualified Data.ByteString as BS

import Network.HTTP.Client

import App


fetchHTTP :: Manager -> String -> AppM BS.ByteString
fetchHTTP manager address = do
    request <- parseUrl address
    chunks <- liftIO $ withResponse request manager (brConsume . responseBody)
    return $ BS.concat chunks
