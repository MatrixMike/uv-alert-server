{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec where

{- Test Utils module -}
import Utils

import Test.Hspec

type DSeries = [(Double, Double)]

spec :: Spec
spec = do
  describe "extrapolate" $ do
    let extrapolateExample = extrapolate (10, 0) (20, 1) :: Double -> Double
    it "works at the left point" $ extrapolateExample 0 `shouldBe` 10
    it "works at the right point" $ extrapolateExample 1 `shouldBe` 20
    it "works at the middle point" $ extrapolateExample 0.5 `shouldBe` 15
    it "works at a different point" $ extrapolateExample 2 `shouldBe` 30
  describe "findValueMonotonic" $ do
    let series = [(0, 10), (1, 20), (2, 40)] :: DSeries
    it "works at the left point" $ do
      findValueMonotonic 10 series `shouldBe` (Just 0)
    it "works before the left point" $ do
      findValueMonotonic 5 series `shouldBe` (Just 0)
    it "works for the middle point" $ do
      findValueMonotonic 20 series `shouldBe` (Just 1)
    it "extrapolates midway" $ do
      findValueMonotonic 15 series `shouldBe` (Just 0.5)
      findValueMonotonic 25 series `shouldBe` (Just 1.25)
    it "works at the right point" $ do
      findValueMonotonic 40 series `shouldBe` (Just 2)
    it "is Nothing beyond the right point" $ do
      findValueMonotonic 50 series `shouldBe` Nothing
  describe "findIntervals" $ do
    context "with no peaks" $ do
      it "finds no intervals" $ do
        findIntervals 0 ([(0, -1), (2, -1)] :: DSeries) `shouldBe` []
    context "with one peak" $ do
      it "finds the positive interval" $ do
        findIntervals 0 ([(0, -1), (2, 1), (4, -1)] :: DSeries) `shouldBe`
          [(1, 3)]
    context "with two peaks" $ do
      it "finds the positive intervals" $ do
        findIntervals 0 ([(0, -1), (2, 1), (4, -1), (6, 1), (8, -1)] :: DSeries) `shouldBe`
          [(1, 3), (5, 7)]
    context "with borderline cases" $ do
      it "finds the positive intervals" $ do
        let series =
              [ (0, -1)
              , (2, 1)
              , (4, -1)
              , (5, 0)
              , (6, -1)
              , (8, 1)
              , (9, 0)
              , (10, 1)
              , (12, -1)
              ] :: DSeries
        let expected = [(1, 3), (5, 5), (7, 11)]
        findIntervals 0 series `shouldBe` expected
    context "with the whole interval matching" $ do
      it "returns the whole interval as one" $ do
        findIntervals 0 ([(10, 1), (20, 2)] :: DSeries) `shouldBe` [(10, 20)]
  describe "removeAccents" $ do
    it "removes accents from Ōsaka" $ removeAccents "Ōsaka" `shouldBe` "Osaka"
    it "removes accents from Kyūshū" $
      removeAccents "Kyūshū" `shouldBe` "Kyushu"
    it "leaves unaccented strings alone" $
      removeAccents "Seoul" `shouldBe` "Seoul"
    it "works on non-letters" $ do
      let testStr = "Words & 1234 =+-"
      removeAccents testStr `shouldBe` testStr
