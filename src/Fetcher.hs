module Fetcher where

import Control.Concurrent

import Control.Lens

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.List
import qualified Data.Set as S
import Data.Time.Clock

import GHC.Exts (groupWith)

import Types
import Types.Config

updateInterval :: Int -- microseconds
updateInterval = 3600 * 1000000

runFetcher :: Config -> IO ()
runFetcher = runReaderT fetcher

fetcher :: AppM ()
fetcher =
  forever $ do
    fetchers <- asks coFetchers
    fetchAll fetchers
    removeOldM
    liftIO $ threadDelay updateInterval

fetchAll :: [Fetcher] -> AppM ()
fetchAll fs =
  forM_ fs $ \f -> do
    logStr $ "Fetching from " ++ fName f ++ "..."
    newForecasts <- fFetch f
    logStr $ "Added " ++ show (length newForecasts) ++ " forecasts."
    stateM $ stForecasts %= S.union (S.fromList newForecasts)

removeOldM :: AppM ()
removeOldM = do
  oldCount <- stateM $ uses stForecasts length
  now <- liftIO getCurrentTime
  stateM $ stForecasts %= removeOld now
  newCount <- stateM $ uses stForecasts length
  logStr $
    "Removed " ++
    show (oldCount - newCount) ++ " forecasts, " ++ show newCount ++ " remain."

removeOld :: UTCTime -> S.Set Forecast -> S.Set Forecast
removeOld now =
  S.fromList . filterBestEachDay . filter (isRecent now) . S.toList

-- Leave only the latest forecast for each day
filterBestEachDay :: [Forecast] -> [Forecast]
filterBestEachDay = map (maximumBy compareUpdated) . groupWith forecastKey
  where
    forecastKey fc = (fc ^. fcLocation, fc ^. fcDate)
