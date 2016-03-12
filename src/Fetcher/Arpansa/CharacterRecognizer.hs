module Fetcher.Arpansa.CharacterRecognizer where

{- Recognize characters in ARPANSA forecast images -}

import Codec.Picture

import Control.Monad

import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe

import Fetcher.Arpansa.Base
import Fetcher.Arpansa.Characters

isBackground :: PixelRGB8 -> Bool
isBackground (PixelRGB8 r g b) = all (< 110) [r, g, b]

verticalCharLine :: DynamicImage -> ImageCoord -> [PixelRGB8]
verticalCharLine (ImageRGB8 img) (left, top) = [pixelAt img left y | y <- [top..bottom]]
    where bottom = top + charHeight - 1

showMatrix :: CharacterMatrix -> String
showMatrix = unlines . map (map showPixel)
    where showPixel True = 'x'
          showPixel False = ' '

diffChars :: CharacterMatrix -> CharacterMatrix -> Int
diffChars = diff `on` concat
    where diff a b = length $ filter id $ zipWith (/=) a b

matrixFromImage :: DynamicImage -> ImageCoord -> CharacterMatrix
matrixFromImage (ImageRGB8 img) (left, top) = [[(not . isBackground) (pixelAt img x y) | x <- [left..right]] | y <- [top..bottom]]
    where right = left + charWidth - 1
          bottom = top + charHeight - 1

-- Recognize a character in the image, given the top-left corner
charAt :: DynamicImage -> ImageCoord -> Maybe Char
charAt img coords = if maxFit < 5 then Just bestCharacter else Nothing
    where (maxFit, bestCharacter) = minimumBy (compare `on` fst) characterFits
          characterFits = map fitCharacter characters
          fitCharacter (example, char) = (diffChars actual example, char)
          actual = matrixFromImage img coords

-- Filter the elements that start the runs of satisfying the predicate
-- starts even [2, 4, 5, 3, 0, 8, 6, 1, 8, 8] = [2, 0, 8]
starts :: (a -> Bool) -> [a] -> [a]
starts pred = map head . filter (not . null) . splitWhen (not . pred)

-- Recognize a string starting at the coordinate
stringAt :: ImageCoord -> DynamicImage -> String
stringAt (left, top) dimg@(ImageRGB8 img) = catMaybes $ map (charAt dimg) characterStarts
    where positions = [(x, top) | x <- [left..imageWidth img - 1]]
          hasCharacter = not . all isBackground . verticalCharLine dimg
          characterStarts = starts hasCharacter positions
