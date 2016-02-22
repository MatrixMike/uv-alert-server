{-# Language DataKinds #-}
{-# Language TypeOperators #-}
module API where

import Servant

import Types
import Types.Location


type API = "register-app" :> ReqBody '[FormUrlEncoded] AppKey :> Post '[JSON] ()
      :<|> "forecast" :> Capture "location" Location :> Get '[JSON] [Forecast]
      :<|> "locations" :> Get '[JSON] [Location]

api :: Proxy API
api = Proxy
