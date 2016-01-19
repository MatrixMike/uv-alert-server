{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestFetcherArpansa where

import Codec.Picture

import qualified Data.ByteString as BS

import Fetcher.Arpansa

import Test.Framework


testImage :: IO DynamicImage
testImage = do
    bytes <- BS.readFile "test/mel_rt.gif"
    let (Right image) = decodeImage bytes
    return image

test_selectPixels = do
    img <- testImage
    let graphLine = selectPixels forecastLineColor img
    assertBool $ (274, 337) `elem` graphLine
    assertBool $ not $ (272, 336) `elem` graphLine
