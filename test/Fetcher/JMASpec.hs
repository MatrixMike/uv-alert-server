module Fetcher.JMASpec where

import Codec.Picture.Types

import Control.Lens

import Data.Time
import Data.Time.LocalTime.TimeZone.Series

import Fetcher.JMA
import Fetcher.JMA.Cities
import Types
import Types.Location
import Types.Location.TimeZones

import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)

import Images
import Misc

testImageName :: (Show a, Ord a, Num a) => a -> [Char]
testImageName i' = "jma/201607220600-" ++ padShow i' ++ ".png"
  where
    padShow i
      | i < 10 = "0" ++ show i
      | otherwise = show i

testImage :: Integer -> SpecM a DynamicImage
testImage = loadImage . testImageName

testImages :: SpecM a [DynamicImage]
testImages = mapM loadImage $ map testImageName [0 .. 12 :: Int]

japanTime :: Day -> Int -> Int -> UTCTime
japanTime date hour minute =
  localTimeToUTC' japanTZ $ LocalTime date $ TimeOfDay hour minute 0

spec :: Spec
spec = do
  describe "japanTime" $ do
    let Just date = fromGregorianValid 2016 05 20
    it "returns the expected UTC time" $ do
      japanTime date 09 30 `shouldBe`
        UTCTime date (timeOfDayToTime $ TimeOfDay 00 30 00)
  describe "imageNameTime" $ do
    let Just date = fromGregorianValid 2016 05 20
        testTime = japanTime date
        Just tomorrow = fromGregorianValid 2016 05 21
        testTimeTomorrow = japanTime tomorrow
        expected baseName =
          "http://www.jma.go.jp/en/uv/imgs/uv_color/forecast/000/" ++
          baseName ++ ".png"
    context "in the morning" $ do
      it "returns the last evening image name" $ do
        imageNameTime (testTime 03 11) 01 `shouldBe`
          (expected "201605191800-01", testTime 07 00)
    context "during the day" $ do
      it "returns the morning image name" $ do
        imageNameTime (testTime 08 25) 02 `shouldBe`
          (expected "201605200600-02", testTime 08 00)
    context "in the evening" $ do
      it "returns the evening image name" $ do
        imageNameTime (testTime 19 31) 03 `shouldBe`
          (expected "201605201800-03", testTimeTomorrow 09 00)
  describe "imageUVLevel" $ do
    img1 <- testImage 1
    img4 <- testImage 4
    img6 <- testImage 6
    img7 <- testImage 7
    context "on clear pixels" $ do
      it "returns the correct UV level" $ do
        imageUVLevel (ImageCoord 322 203) img1 `shouldBe` Just (UVLevel 0)
        imageUVLevel (ImageCoord 279 213) img1 `shouldBe` Just (UVLevel 1)
        imageUVLevel (ImageCoord 323 214) img4 `shouldBe` Just (UVLevel 2)
        imageUVLevel (ImageCoord 107 280) img4 `shouldBe` Just (UVLevel 3)
        imageUVLevel (ImageCoord 330 194) img4 `shouldBe` Just (UVLevel 4)
        imageUVLevel (ImageCoord 290 173) img4 `shouldBe` Just (UVLevel 6)
        imageUVLevel (ImageCoord 286 199) img4 `shouldBe` Just (UVLevel 7)
        imageUVLevel (ImageCoord 303 206) img4 `shouldBe` Just (UVLevel 8)
        imageUVLevel (ImageCoord 240 221) img6 `shouldBe` Just (UVLevel 9)
        imageUVLevel (ImageCoord 160 258) img6 `shouldBe` Just (UVLevel 10)
        imageUVLevel (ImageCoord 137 322) img6 `shouldBe` Just (UVLevel 11)
        imageUVLevel (ImageCoord 63 426) img7 `shouldBe` Just (UVLevel 12)
                -- TODO: No 13 on test images
    context "on black pixels" $ do
      it "returns the nearest UV level" $ do
        imageUVLevel (ImageCoord 211 244) img4 `shouldBe` Just (UVLevel 6)
        imageUVLevel (ImageCoord 200 242) img6 `shouldBe` Just (UVLevel 5)
  describe "imageUVLevels" $ do
    imgs <- testImages
    it "returns the level series" $ do
      imageUVLevels (ImageCoord 324 212) imgs `shouldBe`
        Just (map UVLevel [0, 0, 1, 1, 2, 2, 2, 4, 3, 2, 1, 0, 0])
      imageUVLevels (ImageCoord 192 238) imgs `shouldBe`
        Just (map UVLevel [0, 1, 2, 4, 4, 5, 5, 5, 4, 3, 2, 1, 0])
  describe "forecast" $ do
    imgs <- testImages
    let Just date = fromGregorianValid 2016 05 20
    let times = map (\hour -> japanTime date hour 00) [6 .. 18]
    let loc = Location "Japan" "Tokyo" "Tokyo" (latlon 35.683333 139.683333) japanTZ
    let now = japanTime date 03 04
    let Just fc = forecast now (zip imgs times) loc
    it "has the specified location" $ fc ^. fcLocation `shouldBe` withoutCoordinates loc
    it "stores the day" $ fc ^. fcDate `shouldBe` date
    it "calculates the maximum level" $ fc ^. fcMaxLevel `shouldBe` UVLevel 4
    it "calculates the alert times" $ do
      let [alert] = fc ^. fcAlerts
      alert ^. alertStart `shouldSatisfy`
        (between (TimeOfDay 12 25 0) (TimeOfDay 12 35 0))
      alert ^. alertEnd `shouldSatisfy`
        (between (TimeOfDay 13 55 0) (TimeOfDay 14 05 0))
    it "stores the updated time" $ fc ^. fcUpdated `shouldBe` now
  describe "imageCoord" $ do
    it "finds Tokyo" $ do
      let tokyo = latlon 35.683333 139.683333
      imageCoord tokyo `shouldBe` ImageCoord 324 213
    it "finds Hiroshima" $ do
      let hiroshima = latlon 34.385278 132.455278
      imageCoord hiroshima `shouldBe` ImageCoord 191 240
    it "find Sapporo" $ do
      let sapporo = latlon 43.066667 141.35
      imageCoord sapporo `shouldBe` ImageCoord 355 64
