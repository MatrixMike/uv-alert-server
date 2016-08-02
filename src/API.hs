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


-- Serve JSON with an explicit 'charset' in Content-Type
-- Servant should do that:
-- https://github.com/haskell-servant/servant/issues/559
data UTF8JSON = UTF8JSON

instance Accept UTF8JSON where
      contentType _ = "application" // "json" /: ("charset", "utf-8")

instance ToJSON a => MimeRender UTF8JSON a where
  mimeRender _ = encode

type API = "register-app" :> ReqBody '[FormUrlEncoded] AppKey :> Post '[UTF8JSON] ()
      :<|> "forecast" :> Capture "location" Location :> Get '[UTF8JSON] [Forecast]
      :<|> "locations" :> Get '[UTF8JSON] [Location]

api :: Proxy API
api = Proxy
