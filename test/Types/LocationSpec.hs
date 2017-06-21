{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.LocationSpec where

{- Test Types.Location module -}

import Servant

import Test.Hspec
import Test.QuickCheck

import Types.Location

instance Arbitrary Location where
  arbitrary = Location <$> arbitraryAlpha <*> arbitraryAlpha <*> arbitraryAlpha
    where
      arbitraryAlpha = listOf1 $ choose ('a', 'z')

spec :: Spec
spec = do
  describe "Location" $ do
    it "can be converted to an HTTP URL part" $ do
      let loc = Location "Australia" "Victoria" "Melbourne"
      toQueryParam loc `shouldBe` "Melbourne, Victoria, Australia"
    it "can be converted to/from an HTTP URL part" $
      property $ \loc ->
        parseQueryParam (toQueryParam loc) === Right (loc :: Location)
