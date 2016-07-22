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
