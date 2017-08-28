{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Integration.ServerSpec where

import Server
import Types.Config
import Types.Location

import Integration.Base

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = do
  config <- runIO testConfig
  let locations :: [LocationCoordinates]
      locations =
        [ Location "Australia" "Victoria" "Melbourne" (latlon (-37.73) 145.1)
        , Location "Australia" "New South Wales" "Sydney" (latlon (-34.04) 151.1)
        , Location "Japan" "Hyōgo" "Himeji" (latlon 34.8086 134.7384)
        ]
  let testFetcher =
        Fetcher
        {fName = "test fetcher", fFetch = return [], fLocations = locations}
  let testCfg = config {coFetchers = [testFetcher]}
  with (return $ app testCfg) $ do
    describe "GET /locations" $ do
      it "responds with locations JSON" $ do
        get "/locations" `shouldRespondWith`
          [json|
               [
                 { id: "Australia-New_South_Wales-Sydney"
                 , city: "Sydney"
                 , region: "New South Wales"
                 , country: "Australia"
                 , location: { lat: -34.04,"lon": 151.1}}
               , { id: "Australia-Victoria-Melbourne"
                 , city: "Melbourne"
                 , region: "Victoria"
                 , country: "Australia"
                 , location: { lat: -37.73,"lon": 145.1}}
               , { id: "Japan-Hyogo-Himeji"
                 , city: "Himeji"
                 , region: "Hyōgo"
                 , country: "Japan"
                 , location: { lat: 34.8086,"lon": 134.7384}}
               ]
               |]
          {matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]}
    describe "GET /forecast/Melbourne, Victoria, Australia" $ do
      it "should respond with the found forecasts" $ do
        get "/forecast/Melbourne, Victoria, Australia" `shouldRespondWith`
          [json| [] |]
          {matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]}
