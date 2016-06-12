module Main where

import Control.Concurrent

import Network.Wai.Handler.Warp

import System.IO

import App
import Fetcher
import Server
import Types.Config

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    config <- initConfig
    let theApp = app config
    _ <- forkIO $ runFetcher config
    run (coListenPort config) theApp
