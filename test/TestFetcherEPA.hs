module TestFetcherEPA where

import Control.Lens

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Fetcher.EPA
import Types
import Types.Location

import Test.Hspec


spec :: Spec
spec = do
    let levels = map UVLevel
    describe "firstAlertTime" $ do
        it "should interpolate the level" $ do
            firstAlertTime (levels [0, 0, 2, 6]) `shouldBe` Just 2.25
        it "should start straight away if the alert is already on" $ do
            firstAlertTime (levels [5, 5, 5]) `shouldBe` Just 0
        it "should be Nothing if there is no alert" $ do
            firstAlertTime (levels [0, 0, 2, 0]) `shouldBe` Nothing

    describe "lastAlertTime" $ do
        it "should interpolate the level" $ do
            lastAlertTime (levels [0, 5, 4, 2, 0]) `shouldBe` Just 2.5
        it "should indicate the last hour when the alert is always on" $ do
            lastAlertTime (levels [5, 5, 5]) `shouldBe` Just 2
        it "should be Nothing if there is no alert" $ do
            lastAlertTime (levels [0, 0, 2, 0]) `shouldBe` Nothing
