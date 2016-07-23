module TestFetcherJMA where

import Data.Time
import Data.Time.LocalTime.TimeZone.Series

import Fetcher.JMA
import Types

import Test.Hspec

import Images


testImageName i = "jma/201607220600-" ++ padShow i ++ ".png"
    where padShow i | i < 10 = "0" ++ show i
                    | otherwise = show i


testImage = loadImage . testImageName

testImages = mapM loadImage $ map testImageName [0..12]


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
    describe "imageUVLevel" $ do
        img1 <- testImage 1
        img4 <- testImage 4
        img6 <- testImage 6
        img7 <- testImage 7
        context "on clear pixels" $ do
            it "returns the correct UV level" $ do
                imageUVLevel 322 203 img1 `shouldBe` Just (UVLevel 0)
                imageUVLevel 279 213 img1 `shouldBe` Just (UVLevel 1)
                imageUVLevel 323 214 img4 `shouldBe` Just (UVLevel 2)
                imageUVLevel 107 280 img4 `shouldBe` Just (UVLevel 3)
                imageUVLevel 330 194 img4 `shouldBe` Just (UVLevel 4)
                imageUVLevel 290 173 img4 `shouldBe` Just (UVLevel 6)
                imageUVLevel 286 199 img4 `shouldBe` Just (UVLevel 7)
                imageUVLevel 303 206 img4 `shouldBe` Just (UVLevel 8)
                imageUVLevel 240 221 img6 `shouldBe` Just (UVLevel 9)
                imageUVLevel 160 258 img6 `shouldBe` Just (UVLevel 10)
                imageUVLevel 137 322 img6 `shouldBe` Just (UVLevel 11)
                imageUVLevel  63 426 img7 `shouldBe` Just (UVLevel 12)
                -- TODO: No 13 on test images
