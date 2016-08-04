{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Data.Aeson (encode, ToJSON)

import Network.HTTP.Media hiding (Accept)

import Servant

import Types
import Types.Location


type API = "register-app" :> ReqBody '[FormUrlEncoded] AppKey :> Post '[JSON] ()
      :<|> "forecast" :> Capture "location" Location :> Get '[JSON] [Forecast]
      :<|> "locations" :> Get '[JSON] [Location]

api :: Proxy API
api = Proxy
