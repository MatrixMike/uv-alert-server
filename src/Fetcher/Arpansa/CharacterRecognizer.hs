module Fetcher.Arpansa.CharacterRecognizer where

{- Recognize characters in ARPANSA forecast images -}

import Codec.Picture

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Calendar
import Data.Tuple

import Fetcher.Arpansa.Base
import Fetcher.Arpansa.Characters
import Utils

isBackground :: PixelRGB8 -> Bool
isBackground (PixelRGB8 r g b) = maximum [r, g, b] < 240

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

dateStringCoord :: ImageCoord
dateStringCoord = (176, 72)

months :: M.Map String Int
months = M.fromList $ map swap $ zip [1..] [ "January"
                                           , "February"
                                           , "March"
                                           , "April"
                                           , "May"
                                           , "June"
                                           , "July"
                                           , "August"
                                           , "September"
                                           , "October"
                                           , "November"
                                           , "December"
                                           ]

splitWhenChanges :: Eq b => (a -> b) -> [a] -> [[a]]
splitWhenChanges _ [] = []
splitWhenChanges fn (a:as) = (a:start):splitWhenChanges fn rest
    where sameVal a' = fn a' == fn a
          (start, rest) = break (not . sameVal) as

-- Parse the date on the graph
parseDate :: DynamicImage -> Either String Day
parseDate img = do
    let dateString = stringAt dateStringCoord img
    [_, dayString, monthString, yearString] <- case splitWhenChanges isDigit dateString of
                                                    res@[_, _, _, _] -> return res
                                                    _ -> error $ "Invalid date parsed: " ++ dateString
    day <- readEither "day" dayString
    month <- maybeToEither "month" $ M.lookup (drop 2 monthString) months
    year <- readEither "year" yearString
    maybeToEither "invalid date" $ fromGregorianValid year month day
