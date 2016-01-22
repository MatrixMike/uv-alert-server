{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestFetcher where

import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Data
import Fetcher

import Test.Framework

test_removeOld = do
        let forecasts = [ dummyFc "Melbourne" day1 (morning day1)
                        , dummyFc "Melbourne" day1 (evening day1)
                        , dummyFc "Melbourne" day2 (evening day1)
                        , dummyFc "Melbourne" day2 (morning day2)
                        ]
        assertEqual (S.fromList [ forecasts !! 1, forecasts !! 3 ]) $
            removeOld (evening day1) $ S.fromList forecasts
    where dummyFc loc day updated = Forecast { location = Location loc
                                             , date = day
                                             , alertStart = TimeOfDay 8 0 0
                                             , alertEnd = TimeOfDay 16 0 0
                                             , maxLevel = UVLevel 10
                                             , fcUpdated = updated
                                             }
          Just day1 = fromGregorianValid 2016 01 14
          Just day2 = fromGregorianValid 2016 01 15
          Just day3 = fromGregorianValid 2016 01 16
          morning day = UTCTime day (timeOfDayToTime $ TimeOfDay 7 0 0)
          evening day = UTCTime day (timeOfDayToTime $ TimeOfDay 18 0 0)
