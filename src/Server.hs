{-# Language OverloadedStrings #-}
module Server where

import Control.Concurrent.MVar

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Servant

import API
import Data


-- TODO: Probably needs a database
data Store = Store { appKeys :: [AppKey]
                   , forecasts :: [Forecast]
                   }

emptyStore :: Store
emptyStore = Store [] []

data Config = Config { coStore :: MVar Store }

type AppT m = ReaderT Config m

type AppM = AppT (EitherT ServantErr IO)

stateM :: MonadIO m => State Store a -> AppT m a
stateM fn = do
        store <- asks coStore
        liftIO $ modifyMVar store $ return . fn'
    where fn' s = let (a, s') = runState fn s in (s', a)

server :: ServerT API AppM
server = registerApp :<|> getForecast

registerApp :: AppKey -> AppM ()
registerApp key = do
    stateM $ modify $
        \store -> store { appKeys = appKeys store ++ [key] }

getForecast :: Location -> AppM Forecast
getForecast loc = do
    forecasts <- stateM $ gets forecasts
    let locForecasts = filter ((== loc) . location) forecasts
    case locForecasts of
        [] -> lift $ left $ err404 { errBody = "Location not found" }
        fc:_ -> return fc
