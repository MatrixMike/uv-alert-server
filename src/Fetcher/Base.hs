module Fetcher.Base where

{- Base functions for all the UV level sources -}

import Control.Exception.Lifted

import Network.HTTP.Client (HttpException)

import App
import Types


data Fetcher = Fetcher { fName :: String
                       , fFetch :: AppM [Forecast]
                       }

logErrorStr :: (Monoid a, Show c) => c -> String -> AppM a
logErrorStr context err = do
    logStr $ "Error fetching " ++ show context ++ ": " ++ err
    return mempty

logIOError :: (Monoid a, Show c) => c -> IOError -> AppM a
logIOError context err = logErrorStr context (show err)

logHttpError :: (Monoid a, Show c) => c -> HttpException -> AppM a
logHttpError context err = logErrorStr context (show err)

logErrors :: (Monoid a, Show c) => c -> AppM a -> AppM a
logErrors context = handle (logIOError context) . handle (logHttpError context)
