module TestFetcherJMA where

import Data.Time
import Data.Time.LocalTime.TimeZone.Series

import Fetcher.JMA

import Test.Hspec

import Images


testImages = mapM loadImage $ map testImageName [0..12]
    where testImageName i = "jma/201607220600-" ++ padShow i ++ ".png"
          padShow i | i < 10 = "0" ++ show i
                    | otherwise = show i


japanTime :: Day -> Int -> Int -> UTCTime
japanTime date hour minute = localTimeToUTC' jst $ LocalTime date $ TimeOfDay hour minute 0


spec :: Spec
spec = do
    describe "japanTime" $ do
        let Just date = fromGregorianValid 2016 05 20
        it "returns the expected UTC time" $ do
            japanTime date 09 30 `shouldBe` UTCTime date (timeOfDayToTime $ TimeOfDay 00 30 00)
    describe "imageName" $ do
        let Just date = fromGregorianValid 2016 05 20
            testTime = japanTime date
            expected baseName = "http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/" ++ baseName ++ ".png"
        context "in the morning" $ do
            it "returns the last evening image name" $ do
                imageName (testTime 03 00) 01 `shouldBe` expected "201605191800-01"
            it "returns the morning image name" $ do
                imageName (testTime 08 00) 01 `shouldBe` expected "201605200600-01"
            it "returns the evening image name" $ do
                imageName (testTime 19 00) 01 `shouldBe` expected "201605201800-01"
