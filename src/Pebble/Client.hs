{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeOperators #-}
module Pebble.Client where

import Control.Monad.Trans.Either

import Data.Text

import Pebble.Types

import Servant
import Servant.Client


type PebbleAPI = "v1" :>
    (    "user" :> Header "X-User-Token" UserToken :>
                     (    PinAPI
                     :<|> "subscriptions" :> Get '[JSON] Topics
                     )
    :<|> "shared" :> Header "X-API-Key" APIKey :> Header "X-Pin-Topics" Topics :> PinAPI
    )

type PinAPI = "pins" :> Capture "pin-id" String :>
                  (    ReqBody '[JSON] Pin :> Put '[PlainText] Text
                  :<|> Delete '[PlainText] Text
                  )

api :: Proxy PebbleAPI
api = Proxy

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "timeline-api.getpebble.com" 443

userClient :<|> sharedClient = client api baseUrl

putUserPin :: Maybe UserToken -> String -> Pin -> EitherT ServantError IO Text
putUserPin token id_ = putPin
    where userPinClient :<|> _ = userClient token
          putPin :<|> _ = userPinClient id_

deleteUserPin :: Maybe UserToken -> String -> EitherT ServantError IO Text
deleteUserPin token id_ = deletePin
    where userPinClient :<|> _ = userClient token
          _ :<|> deletePin = userPinClient id_

getUserSubscriptions :: Maybe UserToken -> EitherT ServantError IO Topics
getUserSubscriptions token = getSubscriptions
    where _ :<|> getSubscriptions = userClient token

putSharedPin :: Maybe APIKey -> Maybe Topics -> String -> Pin -> EitherT ServantError IO Text
putSharedPin key topics id_ = putPin
    where putPin :<|> _ = sharedClient key topics id_

deleteSharedPin :: Maybe APIKey -> Maybe Topics -> String -> EitherT ServantError IO Text
deleteSharedPin key topics id_ = deletePin
    where _ :<|> deletePin = sharedClient key topics id_
