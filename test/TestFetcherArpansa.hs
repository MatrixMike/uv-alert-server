{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestFetcherArpansa where

import Codec.Picture

import qualified Data.ByteString as BS
import Data.Time.LocalTime

import Data
import Fetcher.Arpansa

import Test.Framework


testImage :: IO DynamicImage
testImage = do
    bytes <- BS.readFile "test/mel_rt.gif"
    let (Right image) = decodeImage bytes
    return image

test_selectPixels = do
    img <- testImage
    let graphLine = selectForecastGraph img
    assertBool $ (274, 337) `elem` graphLine
    assertBool $ not $ (272, 336) `elem` graphLine
    assertBool $ not $ (755, 308) `elem` graphLine
    assertBool $ not $ (215, 541) `elem` graphLine

test_extrapolate = do
    let extrapolateExample = extrapolate (0, 10) (1, 20)
    assertEqual 10 $ extrapolateExample 0
    assertEqual 20 $ extrapolateExample 1
    assertEqual 30 $ extrapolateExample 2

test_graphLevel = do
    assertEqual (UVLevel 10) (graphLevel (0, 231))
    assertEqual (UVLevel 5) (graphLevel (0, 336))

test_graphTimeOfDay = do
        assertEqual (TimeOfDay 11 0 0) $ roundTime $ graphTimeOfDay (312, 0)
        assertEqual (TimeOfDay 17 0 0) $ roundTime $ graphTimeOfDay (586, 0)
    where roundTime (TimeOfDay h m _) = TimeOfDay h m 0
