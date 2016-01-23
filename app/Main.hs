module Main where

import Control.Concurrent

import Network.Wai.Handler.Warp

import System.IO

import App
import Fetcher
import Server

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    config <- initConfig
    let theApp = app config
    forkIO $ runFetcher config
    run (coListenPort config) theApp
