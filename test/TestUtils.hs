{-# LANGUAGE OverloadedStrings #-}
module TestUtils where

{- Test Utils module -}

import Utils

import Test.Hspec


spec :: Spec
spec = do
    describe "extrapolate" $ do
        let extrapolateExample = extrapolate (0, 10) (1, 20)
        it "works at the left point" $ do
            extrapolateExample 0 `shouldBe` 10
        it "works at the right point" $ do
            extrapolateExample 1 `shouldBe` 20
        it "works at a different point" $ do
            extrapolateExample 2 `shouldBe` 30
    describe "removeAccents" $ do
        it "removes accents from Ōsaka" $
            removeAccents "Ōsaka" `shouldBe` "Osaka"
        it "removes accents from Kyūshū" $
            removeAccents "Kyūshū" `shouldBe` "Kyushu"
        it "leaves unaccented strings alone" $
            removeAccents "Seoul" `shouldBe` "Seoul"
        it "works on non-letters" $ do
            let testStr = "Words & 1234 =+-"
            removeAccents testStr `shouldBe` testStr
