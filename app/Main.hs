module Main where

import Control.Concurrent

import Network.Wai.Handler.Warp

import App
import Fetcher
import Server

main :: IO ()
main = do
    config <- initConfig
    let theApp = app config
    forkIO $ runFetcher config
    run 8000 theApp
