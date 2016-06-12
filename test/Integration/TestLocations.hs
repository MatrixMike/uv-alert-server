{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Integration.TestLocations where

import Control.Lens

import Data.Maybe
import Data.Time.Calendar
import qualified Data.Set as S

import Server
import Types
import Types.Config
import Types.Location

import Integration.Base

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON


spec = do
    config <- runIO testConfig
    let locations = [ Location "Australia" "Victoria" "Melbourne"
                    , Location "Australia" "New South Wales" "Sydney"
                    ]
    let testFetcher = Fetcher { fName = "test fetcher"
                              , fFetch = return []
                              , fLocations = locations
                              }
    let testConfig = config { coFetchers = [testFetcher] }
    with (return $ app testConfig) $ do

        describe "GET /locations" $ do
            it "responds with locations JSON" $ do
                get "/locations" `shouldRespondWith` [json|[ { city: "Sydney"
                                                             , region: "New South Wales"
                                                             , country: "Australia"}
                                                           , { city: "Melbourne"
                                                             , region: "Victoria"
                                                             , country: "Australia"}
                                                           ]|]
