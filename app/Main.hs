module Main where

import Network.Wai.Handler.Warp

import App

main :: IO ()
main = do
    app <- app
    run 8000 app
