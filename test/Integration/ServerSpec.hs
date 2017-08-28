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
  let locations =
        [ Location "Australia" "Victoria" "Melbourne"
        , Location "Australia" "New South Wales" "Sydney"
        , Location "Japan" "Hyōgo" "Himeji"
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
          {matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]}
    describe "GET /forecast/Melbourne, Victoria, Australia" $ do
      it "should respond with the found forecasts" $ do
        get "/forecast/Melbourne, Victoria, Australia" `shouldRespondWith`
          [json| [] |]
          {matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]}
