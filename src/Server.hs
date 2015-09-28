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

withStore :: (State Store a) -> AppM a
withStore fn = do
        storeM <- asks coStore
        liftIO $ modifyMVar storeM $ return . runState' fn
    where runState' fn s = let (a, s') = runState fn s in (s', a)

type AppM = ReaderT Config (EitherT ServantErr IO)

server :: ServerT API AppM
server = registerApp :<|> getForecast

registerApp :: AppKey -> AppM ()
registerApp key = do
    withStore $ modify $
        \store -> store { appKeys = appKeys store ++ [key] }

getForecast :: Location -> AppM Forecast
getForecast = undefined
