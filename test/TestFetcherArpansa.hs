module TestFetcherArpansa where

import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Fetcher.Arpansa
import Fetcher.Arpansa.CharacterRecognizer
import Fetcher.Arpansa.Characters
import Types
import Types.Location

import Test.Hspec

import Images
import Misc


morningImage = "arpansa/mel_rt_morning.gif"
eveningImage = "arpansa/mel_rt_evening.gif"
noActualImage = "arpansa/mel_rt_no_actual.gif"
quietImage = "arpansa/mel_rt_quiet.gif"
distinctPeriodsImage = "arpansa/mel_rt_distinct_periods.gif"
perthMarch08Image = "arpansa/per_rt_2016-03-08.gif"
melMarch11Image = "arpansa/mel_rt_2016-03-11.gif"

melbourne = Location "Australia" "Victoria" "Melbourne"

time :: Int -> Int -> TimeOfDay
time h m = TimeOfDay h m 0

spec :: Spec
spec = do
    describe "selectForecastLine" $ do
        img <- loadImage morningImage
        let graphLine = selectForecastLine img
        it "selects the graph point" $ do
            graphLine `shouldSatisfy` (elem (274, 337))
        it "does not select the point which is not in the graph" $ do
            graphLine `shouldSatisfy` (not . elem (272, 336))
        it "does not select the legend points" $ do
            graphLine `shouldSatisfy` (not . elem (755, 308))
        it "does not select the forecast level text" $ do
            graphLine `shouldSatisfy` (not . elem (215, 541))

    describe "selectBestLine" $ do
        img <- loadImage eveningImage
        let graphLine = selectBestLine img
        it "selects the actual line point" $ do
            graphLine `shouldSatisfy` (elem (197, 431))
        it "does not select the forecast line where the actual data exists" $ do
            graphLine `shouldSatisfy` (not . elem (197, 411))
        context "when there is no actual line" $ do
            img <- loadImage noActualImage
            let graphLine = selectBestLine img
            it "selects the forecast line" $ do
                graphLine `shouldSatisfy` (elem (124, 435))

    describe "graphLevel" $ do
        it "works for level 10" $ do
            graphLevel 231 `shouldBe` UVLevel 10
        it "works for level 5" $ do
            graphLevel 336 `shouldBe` UVLevel 5

    describe "graphTimeOfDay" $ do
        let roundTime (TimeOfDay h m _) = TimeOfDay h m 0
        it "works for 11:00" $
            roundTime (graphTimeOfDay 312) `shouldBe` time 11 0
        it "works for 17:00" $
            roundTime (graphTimeOfDay 586) `shouldBe` time 17 0

    describe "charAt" $ do
        img_2016_01_19 <- loadImage morningImage
        img_2016_01_20 <- loadImage eveningImage
        img_2016_03_08 <- loadImage perthMarch08Image
        it "recognizes 0" $ do
            charAt img_2016_01_20 (275, 72) `shouldBe` Just '0'
        it "recognizes 1" $ do
            charAt img_2016_01_20 (402, 72) `shouldBe` Just '1'
        it "recognizes 2" $ do
            charAt img_2016_01_20 (266, 72) `shouldBe` Just '2'
        it "recognizes 6" $ do
            charAt img_2016_01_20 (410, 72) `shouldBe` Just '6'
        it "recognizes 8" $ do
            charAt img_2016_03_08 (248, 72) `shouldBe` Just '8'
        it "recognizes 9" $ do
            charAt img_2016_01_19 (257, 72) `shouldBe` Just '9'

    describe "stringAt" $ do
        img_2016_01_20 <- loadImage eveningImage
        it "recognizes the date string" $ do
            stringAt (176, 72) img_2016_01_20
                `shouldBe` "Wednesday20thJanuary2016"

    describe "characters" $ do
        let known = S.fromList $ map snd characters
        it "should have all the letters to recognize months" $ do
            let needed = S.fromList $ concat $ M.keys months
            S.difference needed known `shouldBe` S.empty
        it "should have all the digits" $ do
            let digits = S.fromList $ ['0'..'9']
            S.difference digits known `shouldBe` S.empty

    describe "parseDate" $ do
        img_2016_03_08 <- loadImage perthMarch08Image
        it "parses the date" $ do
            let Just imgDate = fromGregorianValid 2016 03 08
            parseDate img_2016_03_08 `shouldBe` Right imgDate

    describe "parseGraph" $ do

        let Just test_date = fromGregorianValid 2016 01 01
        let test_time = secondsToDiffTime 0
        let testTime = UTCTime test_date test_time

        context "for a morning image" $ do
            img <- loadImage morningImage
            let (Just fc) = parseGraph melbourne img testTime
            it "stores the city" $
                fc ^. fcLocation . locCity `shouldBe` "Melbourne"
            it "stores the day" $ do
                let Just day = fromGregorianValid 2016 1 19
                fc ^. fcDate `shouldBe` day
            it "calculates the maximum level" $ do
                fc ^. fcMaxLevel `shouldBe` UVLevel 10
            it "calculates the alert times" $ do
                let [alert] = fc ^. fcAlerts
                alert ^. alertStart `shouldSatisfy` (between (time 9 0) (time 9 30))
                alert ^. alertEnd `shouldSatisfy` (between (time 17 40) (time 18 0))
            it "stores the updated time" $ do
                fc ^. fcUpdated `shouldBe` testTime

        context "for an evening image" $ do
            -- This image has the real data overlaid on the forecast
            -- The real UV index was low in the morning, so the alert should be
            -- adjusted
            img <- loadImage eveningImage
            let (Just fc) = parseGraph melbourne img testTime
            it "stores the city" $
                fc ^. fcLocation . locCity `shouldBe` "Melbourne"
            it "stores the day" $ do
                let Just day = fromGregorianValid 2016 1 20
                fc ^. fcDate `shouldBe` day
            it "calculates the maximum level" $ do
                fc ^. fcMaxLevel `shouldBe` UVLevel 12
            it "calculates the alert times" $ do
                let [alert] = fc ^. fcAlerts
                alert ^. alertStart `shouldSatisfy` (between (time 11 0) (time 11 20))
                alert ^. alertEnd `shouldSatisfy` (between (time 17 30) (time 17 50))
            it "stores the updated time" $ do
                fc ^. fcUpdated `shouldBe` testTime

        context "for an image with no high UV level" $ do
            -- This image has been altered to have no alert
            img <- loadImage quietImage
            let Just day = fromGregorianValid 2016 1 20
            it "does not have an alert forecast" $ do
                parseGraph melbourne img testTime `shouldBe` Nothing

        context "for an image with several high intervals" $ do
            -- This image has been altered to have a few intervals of high UV
            -- index
            img <- loadImage distinctPeriodsImage
            let Just day = fromGregorianValid 2016 1 20
            let (Just fc) = parseGraph melbourne img testTime
            it "has a range of alerts" $ do
                length (fc ^. fcAlerts) `shouldBe` 3
                let [alert1, alert2, alert3] = fc ^. fcAlerts
                alert1 ^. alertStart `shouldSatisfy` (between (time 9 50) (time 10 10))
                alert1 ^. alertEnd `shouldSatisfy` (between (time 11 0) (time 11 20))
                alert2 ^. alertStart `shouldSatisfy` (between (time 14 0) (time 14 20))
                alert2 ^. alertEnd `shouldSatisfy` (between (time 16 0) (time 16 20))
                alert3 ^. alertStart `shouldSatisfy` (between (time 16 50) (time 17 10))
                alert3 ^. alertEnd `shouldSatisfy` (between (time 17 30) (time 17 50))
