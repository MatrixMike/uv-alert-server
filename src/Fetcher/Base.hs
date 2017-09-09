module Fetcher.Base where

{- Base functions for all the UV level sources -}
import Control.Applicative
import Control.Exception.Lifted
import Control.Retry

import Data.Default

import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple (JSONException)

import Types.Config

logErrorStr :: (Alternative m, Show c) => c -> String -> AppM (m a)
logErrorStr context err = do
  logStr $ "Error fetching " ++ show context ++ ": " ++ err
  return empty

logIOError :: (Alternative m, Show c) => c -> IOError -> AppM (m a)
logIOError context err = logErrorStr context (show err)

logHttpError :: (Alternative m, Show c) => c -> HttpException -> AppM (m a)
logHttpError context err = logErrorStr context (show err)

logJSONError :: (Alternative m, Show c) => c -> JSONException -> AppM (m a)
logJSONError context err = logErrorStr context (show err)

logErrors :: (Alternative m, Show c) => c -> AppM (m a) -> AppM (m a)
logErrors context =
  handle (logIOError context) .
  handle (logHttpError context) . handle (logJSONError context) . doRetries

doRetries :: AppM a -> AppM a
doRetries act =
  recoverAll def $ \status -> do
    case rsIterNumber status of
      0 -> pure ()
      i -> logStr $ "Retrying, attempt " ++ show i ++ "..."
    act

logEither :: Alternative m => Either String a -> (a -> AppM (m b)) -> AppM (m b)
logEither (Left err) _ = logStr err >> return empty
logEither (Right value) act = act value
