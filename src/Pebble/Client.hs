{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeOperators #-}
module Pebble.Client where

import Control.Monad.Trans.Either

import Pebble.Types

import Servant
import Servant.API.Alternative
import Servant.Client


type PebbleAPI = "v1" :>
    (    "user" :> Header "X-User-Token" UserToken :>
                     (    PinAPI
                     :<|> "subscriptions" :> Get '[JSON] Topics
                     )
    :<|> "shared" :> Header "X-API-Key" APIKey :> Header "X-Pin-Topics" Topics :> PinAPI
    )

type PinAPI = "pins" :> Capture "pin-id" String :>
                  (    ReqBody '[JSON] Pin :> Put '[JSON] ()
                  :<|> Delete '[JSON] ()
                  )

api :: Proxy PebbleAPI
api = Proxy

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "timeline-api.getpebble.com" 443

userClient :<|> sharedClient = client api baseUrl

putUserPin :: Maybe UserToken -> String -> Pin -> EitherT ServantError IO ()
putUserPin token pinId pin = putPin pin
    where userPinClient :<|> _ = userClient token
          putPin :<|> _ = userPinClient pinId

deleteUserPin :: Maybe UserToken -> String -> EitherT ServantError IO ()
deleteUserPin token pinId = deletePin
    where userPinClient :<|> _ = userClient token
          _ :<|> deletePin = userPinClient pinId

getUserSubscriptions :: Maybe UserToken -> EitherT ServantError IO Topics
getUserSubscriptions token = getSubscriptions
    where _ :<|> getSubscriptions = userClient token

putSharedPin :: Maybe APIKey -> Maybe Topics -> String -> Pin -> EitherT ServantError IO ()
putSharedPin key topics pinId pin = putPin pin
    where putPin :<|> _ = sharedClient key topics pinId

deleteSharedPin :: Maybe APIKey -> Maybe Topics -> String -> EitherT ServantError IO ()
deleteSharedPin key topics pinId = deletePin
    where _ :<|> deletePin = sharedClient key topics pinId
