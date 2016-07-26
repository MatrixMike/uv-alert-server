module Fetcher.Base where

{- Base functions for all the UV level sources -}

import Control.Monad
import Control.Exception.Lifted

import Network.HTTP.Client (HttpException)

import Types.Config
import Types


logErrorStr :: (MonadPlus m, Show c) => c -> String -> AppM (m a)
logErrorStr context err = do
    logStr $ "Error fetching " ++ show context ++ ": " ++ err
    return mzero

logIOError :: (MonadPlus m, Show c) => c -> IOError -> AppM (m a)
logIOError context err = logErrorStr context (show err)

logHttpError :: (MonadPlus m, Show c) => c -> HttpException -> AppM (m a)
logHttpError context err = logErrorStr context (show err)

logErrors :: (MonadPlus m, Show c) => c -> AppM (m a) -> AppM (m a)
logErrors context = handle (logIOError context) . handle (logHttpError context)
