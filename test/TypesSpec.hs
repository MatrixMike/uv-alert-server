{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

{- Test Types module -}
import Control.Lens

import Data.Maybe
import Data.Time
import Data.Time.LocalTime.TimeZone.Series

import Types
import Types.Location
import Types.Location.Japan

import Test.Hspec

japanTime :: Day -> Int -> Int -> UTCTime
japanTime date hour minute =
  localTimeToUTC' japanTZ $ LocalTime date $ TimeOfDay hour minute 0

spec :: Spec
spec = do
  describe "buildForecast" $ do
    context "builds the forecast for the right time zone" $ do
      let Just date = fromGregorianValid 2016 09 01
      let atHour h = japanTime date h 0
      let tokyo = Location "Japan" "Tokyo" "Tokyo" ()
      it "is Nothing when there is no alert" $
        buildForecast tokyo (atHour 19) [] `shouldBe` Nothing
      it "has correct date and times when there is an alert" $ do
        let testLevels =
              [ (atHour 6, UVLevel 1)
              , (atHour 7, UVLevel 2)
              , (atHour 8, UVLevel 3)
              , (atHour 9, UVLevel 4)
              , (atHour 10, UVLevel 5)
              , (atHour 11, UVLevel 4)
              , (atHour 12, UVLevel 3)
              , (atHour 13, UVLevel 2)
              , (atHour 14, UVLevel 1)
              ]
        let Just forecast = buildForecast tokyo (atHour 19) testLevels
        (forecast ^. fcDate) `shouldBe`
          (fromJust $ fromGregorianValid 2016 09 01)
