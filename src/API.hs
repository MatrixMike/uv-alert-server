{-# Language DataKinds #-}
{-# Language TypeOperators #-}
module API where

import Servant

import Data


type API = "register-app" :> ReqBody '[FormUrlEncoded] AppKey :> Post '[JSON] ()
      :<|> "forecast" :> Capture "location" Location :> Get '[JSON] Forecast

api :: Proxy API
api = Proxy