module Integration.Base where

import Data.Aeson

import App

withApp :: AppM () -> IO ()
withApp = undefined

getJson :: String -> AppM Value
getJson = undefined
