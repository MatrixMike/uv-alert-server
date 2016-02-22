{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Integration.TestLocations where

import Control.Lens
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Set as S
import qualified Data.Vector as V

import App
import Server
import Types
import Types.Location

import Integration.Base

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON


spec = do
    config <- runIO testConfig
    let dummyForecast loc = Forecast { _fcLocation = loc
                                     , _fcDate = fromJust $ fromGregorianValid 2016 2 1
                                     , _fcAlertStart = undefined
                                     , _fcAlertEnd = undefined
                                     , _fcMaxLevel = UVLevel 10
                                     , _fcUpdated = undefined
                                     }
    let locations = [ Location "Australia" "Victoria" "Melbourne"
                    , Location "Australia" "New South Wales" "Sydney"
                    ]
    runIO $ inApp config $ stateM $ stForecasts .= (S.fromList $ map dummyForecast locations)
    with (return $ app config) $ do

        describe "GET /locations" $ do
            it "responds with locations JSON" $ do
                get "/locations" `shouldRespondWith` [json|[ { city: "Sydney"
                                                             , region: "New South Wales"
                                                             , country: "Australia"}
                                                           , { city: "Melbourne"
                                                             , region: "Victoria"
                                                             , country: "Australia"}
                                                           ]|]
