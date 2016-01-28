{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestFetcherArpansa where

import Codec.Picture

import Control.Lens

import qualified Data.ByteString as BS
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Data
import Fetcher.Arpansa

import Test.Framework


loadImage :: String -> IO DynamicImage
loadImage imageName = do
    bytes <- BS.readFile $ "test/" ++ imageName
    let (Right image) = decodeImage bytes
    return image

morningImage = "mel_rt_morning.gif"
eveningImage = "mel_rt_evening.gif"
noActualImage = "mel_rt_no_actual.gif"

test_selectForecastLine = do
    img <- loadImage morningImage
    let graphLine = selectForecastLine img
    assertBool $ (274, 337) `elem` graphLine
    assertBool $ not $ (272, 336) `elem` graphLine
    -- This point belongs to the legend
    assertBool $ not $ (755, 308) `elem` graphLine
    -- This point belongs to the "Forecast UV Level" text
    assertBool $ not $ (215, 541) `elem` graphLine

test_selectBestLine = do
    img <- loadImage eveningImage
    let graphLine = selectBestLine img
    assertBool $ (197, 431) `elem` graphLine
    -- This point belongs to the forecast line where the actual data exists
    assertBool $ not $ (197, 411) `elem` graphLine

test_selectBestLine_no_actual = do
    img <- loadImage noActualImage
    let graphLine = selectBestLine img
    assertBool $ (124, 435) `elem` graphLine

test_extrapolate = do
    let extrapolateExample = extrapolate (0, 10) (1, 20)
    assertEqual 10 $ extrapolateExample 0
    assertEqual 20 $ extrapolateExample 1
    assertEqual 30 $ extrapolateExample 2

test_graphLevel = do
    assertEqual (UVLevel 10) (graphLevel 231)
    assertEqual (UVLevel 5) (graphLevel 336)

test_graphTimeOfDay = do
        assertEqual (TimeOfDay 11 0 0) $ roundTime $ graphTimeOfDay 312
        assertEqual (TimeOfDay 17 0 0) $ roundTime $ graphTimeOfDay 586
    where roundTime (TimeOfDay h m _) = TimeOfDay h m 0

testTime = UTCTime test_date test_time
    where Just test_date = fromGregorianValid 2016 01 01
          test_time = secondsToDiffTime 0

test_parseGraph = do
        img <- loadImage morningImage
        let fc = parseGraph "Melbourne" day img testTime
        assertEqual "Melbourne" (fc ^. fcLocation . locCity)
        assertEqual day (fc ^. fcDate)
        assertEqual (UVLevel 10) (fc ^. fcMaxLevel)
        let fcStart = fc ^. fcAlertStart
        let fcEnd = fc ^. fcAlertEnd
        assertBool (fcStart > (TimeOfDay 9 0 0) && fcStart < (TimeOfDay 9 30 0))
        assertBool (fcEnd > (TimeOfDay 17 40 0) && fcEnd < (TimeOfDay 18 0 0))
        assertEqual testTime (fc ^. fcUpdated)
    where Just day = fromGregorianValid 2016 1 19

test_parseEveningGraph = do
        -- This image has the real data overlaid on the forecast
        -- The real UV index was low in the morning, so the alert should be
        -- adjusted
        img <- loadImage eveningImage
        let fc = parseGraph "Melbourne" day img testTime
        assertEqual "Melbourne" (fc ^. fcLocation . locCity)
        assertEqual day (fc ^. fcDate)
        assertEqual (UVLevel 12) (fc ^. fcMaxLevel)
        let fcStart = fc ^. fcAlertStart
        let fcEnd = fc ^. fcAlertEnd
        assertBool (fcStart > (TimeOfDay 11 0 0) && fcStart < (TimeOfDay 11 20 0))
        assertBool (fcEnd > (TimeOfDay 17 30 0) && fcEnd < (TimeOfDay 17 50 0))
        assertEqual testTime (fc ^. fcUpdated)
    where Just day = fromGregorianValid 2016 1 20
