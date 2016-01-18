module Fetcher.Base where

{- Base functions for all the UV level sources -}

import Network.URI

import App
import Data


data Fetcher = Fetcher { fName :: String
                       , fFetch :: AppM [Forecast]
                       }

logError :: Monoid a => URI -> IOError -> AppM a
logError address err = do
    logStr $ "Error fetching " ++ show address ++ ": " ++ show err
    return mempty
