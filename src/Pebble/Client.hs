{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Pebble.Client where

import Data.Text

import Pebble.Types

import Servant
import Servant.Client

type PebbleAPI
   = "v1" :> ("user" :> Header "X-User-Token" UserToken :> (PinAPI :<|> "subscriptions" :> Get '[ JSON] Topics) :<|> "shared" :> Header "X-API-Key" APIKey :> Header "X-Pin-Topics" Topics :> PinAPI)

type PinAPI
   = "pins" :> Capture "pin-id" Text :> (ReqBody '[ JSON] Pin :> Put '[ PlainText] Text :<|> Delete '[ PlainText] Text)

api :: Proxy PebbleAPI
api = Proxy

pebbleUrl :: BaseUrl
pebbleUrl = BaseUrl Https "timeline-api.getpebble.com" 443 "/"

userClient :<|> sharedClient = client api

putUserPin :: Maybe UserToken -> Text -> Pin -> ClientM Text
putUserPin token id_ = putPin
  where
    userPinClient :<|> _ = userClient token
    putPin :<|> _ = userPinClient id_

deleteUserPin :: Maybe UserToken -> Text -> ClientM Text
deleteUserPin token id_ = deletePin
  where
    userPinClient :<|> _ = userClient token
    _ :<|> deletePin = userPinClient id_

getUserSubscriptions :: Maybe UserToken -> ClientM Topics
getUserSubscriptions token = getSubscriptions
  where
    _ :<|> getSubscriptions = userClient token

putSharedPin :: Maybe APIKey -> Maybe Topics -> Text -> Pin -> ClientM Text
putSharedPin key topics id_ = putPin
  where
    putPin :<|> _ = sharedClient key topics id_

deleteSharedPin :: Maybe APIKey -> Maybe Topics -> Text -> ClientM Text
deleteSharedPin key topics id_ = deletePin
  where
    _ :<|> deletePin = sharedClient key topics id_
