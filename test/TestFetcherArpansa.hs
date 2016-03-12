module TestFetcherArpansa where

import Codec.Picture

import Control.Lens

import qualified Data.ByteString as BS
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


loadImage imageName = do
    bytes <- runIO $ BS.readFile $ "test/" ++ imageName
    let (Right image) = decodeImage bytes
    return image

morningImage = "mel_rt_morning.gif"
eveningImage = "mel_rt_evening.gif"
noActualImage = "mel_rt_no_actual.gif"
quietImage = "mel_rt_quiet.gif"
perthMarch08Image = "per_rt_2016-03-08.gif"
melMarch11Image = "mel_rt_2016-03-11.gif"

melbourne = Location "Australia" "Victoria" "Melbourne"

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

    describe "extrapolate" $ do
        let extrapolateExample = extrapolate (0, 10) (1, 20)
        it "works at the left point" $ do
            extrapolateExample 0 `shouldBe` 10
        it "works at the right point" $ do
            extrapolateExample 1 `shouldBe` 20
        it "works at a different point" $ do
            extrapolateExample 2 `shouldBe` 30

    describe "graphLevel" $ do
        it "works for level 10" $ do
            graphLevel 231 `shouldBe` UVLevel 10
        it "works for level 5" $ do
            graphLevel 336 `shouldBe` UVLevel 5

    describe "graphTimeOfDay" $ do
        let roundTime (TimeOfDay h m _) = TimeOfDay h m 0
        it "works for 11:00" $
            roundTime (graphTimeOfDay 312) `shouldBe` TimeOfDay 11 0 0
        it "works for 17:00" $
            roundTime (graphTimeOfDay 586) `shouldBe` TimeOfDay 17 0 0

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
        it "should have all the letters to recognize months" $ do
            let known = S.fromList $ map snd characters
            let needed = S.fromList $ concat $ M.keys months
            S.difference needed known `shouldBe` S.empty

    describe "parseDate" $ do
        img_2016_03_08 <- loadImage perthMarch08Image
        it "parses the date" $ do
            let Just imgDate = fromGregorianValid 2016 03 08
            parseDate img_2016_03_08 `shouldBe` Right imgDate

    describe "parseGraph" $ do

        let Just test_date = fromGregorianValid 2016 01 01
        let test_time = secondsToDiffTime 0
        let testTime = UTCTime test_date test_time

        let between :: Ord a => a -> a -> a -> Bool
            between low high value = value > low && value < high

        context "for a morning image" $ do
            let Just day = fromGregorianValid 2016 1 19
            img <- loadImage morningImage
            let (Just fc) = parseGraph melbourne day img testTime
            it "stores the city" $
                fc ^. fcLocation . locCity `shouldBe` "Melbourne"
            it "stores the day" $
                fc ^. fcDate `shouldBe` day
            it "calculates the maximum level" $ do
                fc ^. fcMaxLevel `shouldBe` UVLevel 10
            it "calculates the alert start time" $ do
                fc ^. fcAlertStart `shouldSatisfy` (between (TimeOfDay 9 0 0) (TimeOfDay 9 30 0))
            it "calculates the alert end time" $ do
                fc ^. fcAlertEnd `shouldSatisfy` (between (TimeOfDay 17 40 0) (TimeOfDay 18 0 0))
            it "stores the updated time" $ do
                fc ^. fcUpdated `shouldBe` testTime

        context "for an evening image" $ do
            -- This image has the real data overlaid on the forecast
            -- The real UV index was low in the morning, so the alert should be
            -- adjusted
            let Just day = fromGregorianValid 2016 1 20
            img <- loadImage eveningImage
            let (Just fc) = parseGraph melbourne day img testTime
            it "stores the city" $
                fc ^. fcLocation . locCity `shouldBe` "Melbourne"
            it "stores the day" $
                fc ^. fcDate `shouldBe` day
            it "calculates the maximum level" $ do
                fc ^. fcMaxLevel `shouldBe` UVLevel 12
            it "calculates the alert start time" $ do
                fc ^. fcAlertStart `shouldSatisfy` (between (TimeOfDay 11 0 0) (TimeOfDay 11 20 0))
            it "calculates the alert end time" $ do
                fc ^. fcAlertEnd `shouldSatisfy` (between (TimeOfDay 17 30 0) (TimeOfDay 17 50 0))
            it "stores the updated time" $ do
                fc ^. fcUpdated `shouldBe` testTime

        context "for an image with no high UV level" $ do
            -- This image has been altered to have no alert
            img <- loadImage quietImage
            let Just day = fromGregorianValid 2016 1 20
            it "does not have an alert forecast" $ do
                parseGraph melbourne day img testTime `shouldBe` Nothing
