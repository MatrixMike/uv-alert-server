{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Integration.ServerSpec where

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
  let locations =
        [ Location "Australia" "Victoria" "Melbourne"
        , Location "Australia" "New South Wales" "Sydney"
        , Location "Japan" "Hyōgo" "Himeji"
        ]
  let testFetcher =
        Fetcher
        {fName = "test fetcher", fFetch = return [], fLocations = locations}
  let testConfig = config {coFetchers = [testFetcher]}
  with (return $ app testConfig) $ do
    describe "GET /locations" $ do
      it "responds with locations JSON" $ do
        get "/locations" `shouldRespondWith`
          [json|
               [
                 { id: "Australia-New_South_Wales-Sydney"
                 , city: "Sydney"
                 , region: "New South Wales"
                 , country: "Australia"}
               , { id: "Australia-Victoria-Melbourne"
                 , city: "Melbourne"
                 , region: "Victoria"
                 , country: "Australia"}
               , { id: "Japan-Hyogo-Himeji"
                 , city: "Himeji"
                 , region: "Hyōgo"
                 , country: "Japan"}
               ]
               |]
    describe "GET /forecast/Melbourne, Victoria, Australia" $ do
      it "should respond with the found forecasts" $ do
        get "/forecast/Melbourne, Victoria, Australia" `shouldRespondWith`
          [json| [] |]
