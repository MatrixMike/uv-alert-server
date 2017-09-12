module FetcherSpec where

import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Fetcher
import Types
import Types.Location
import Types.Location.TimeZones

import Test.Hspec

spec :: Spec
spec = do
  describe "removeOld" $ do
    let forecasts =
          [ dummyFc "Melbourne" day1 (morning day1)
          , dummyFc "Melbourne" day1 (evening day1)
          , dummyFc "Melbourne" day2 (evening day1)
          , dummyFc "Melbourne" day2 (morning day2)
          ]
    it "should remove old forecasts" $ do
      (removeOld (morning day1) $ S.fromList forecasts) `shouldBe`
        (S.fromList [forecasts !! 1, forecasts !! 3])
      (removeOld (evening day1) $ S.fromList forecasts) `shouldBe`
        (S.fromList [forecasts !! 3])
      (removeOld (evening day2) $ S.fromList forecasts) `shouldBe`
        (S.fromList [])
  where
    dummyFc loc day updated =
      Forecast
      { _fcLocation = dummyLocation loc
      , _fcDate = day
      , _fcAlerts = [Alert (TimeOfDay 8 0 0) (TimeOfDay 16 0 0)]
      , _fcMaxLevel = UVLevel 10
      , _fcUpdated = updated
      }
    dummyLocation city = Location "Australia" "Victoria" city () vicTZ
    Just day1 = fromGregorianValid 2016 01 14
    Just day2 = fromGregorianValid 2016 01 15
    morning day = UTCTime day (timeOfDayToTime $ TimeOfDay 7 0 0)
    evening day = UTCTime day (timeOfDayToTime $ TimeOfDay 18 0 0)
