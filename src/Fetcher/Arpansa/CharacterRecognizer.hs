module Fetcher.Arpansa.CharacterRecognizer where

{- Recognize characters in ARPANSA forecast images -}

import Codec.Picture

import Control.Monad

import Data.Function
import Data.List
import Data.Maybe

import Fetcher.Arpansa.Base

isBackground :: PixelRGB8 -> Bool
isBackground (PixelRGB8 r g b) = all (< 110) [r, g, b]

-- Character dimensions
charWidth = 8
charHeight = 13

-- Recognize a character in the image, given the top-left corner
-- Skips the background around character
charAt :: ImageCoord -> DynamicImage -> Maybe Char
charAt coord img = do
    preciseCoord <- findCharEdge coord img
    charAtPrecise preciseCoord img

-- Find the bounds of the character, given the top-left corner
findCharEdge :: ImageCoord -> DynamicImage -> Maybe ImageCoord
findCharEdge (left, top) dimg@(ImageRGB8 img) = do
    let right = left + charWidth - 1
    let bottom = top + charHeight - 1
    guard $ right < imageWidth img
    guard $ bottom < imageHeight img
    if all isBackground [pixelAt img left y | y <- [top..bottom]]
       then findCharEdge (left + 1, top) dimg
       else if all isBackground [pixelAt img x top | x <- [left..right]]
                then findCharEdge (left, top + 1) dimg
                else return (left, top)

-- Stored characters for comparison, size charHeight x charWidth
type CharacterMatrix = [[Bool]]

-- Parse a stored character into a matrix of pixels
parseCharacter :: [String] -> CharacterMatrix
parseCharacter = take charHeight . map (map (/= ' ') . take charWidth . (++ (repeat ' ')))

showMatrix :: CharacterMatrix -> String
showMatrix = unlines . map (map showPixel)
    where showPixel True = 'x'
          showPixel False = ' '

digit0 :: CharacterMatrix
digit0 = parseCharacter [ "  xxxx  "
                        , " xx  xx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xx  xx "
                        , "  xxxx  "
                        ]

digit1 = parseCharacter [ "  xx  "
                        , " xxx  "
                        , "xxxx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "xxxxxx"
                        ]

digit2 = parseCharacter [ " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "      xx"
                        , "     xx "
                        , "    xx  "
                        , "  xxx   "
                        , " xx     "
                        , "xx      "
                        , "xxxxxxxx"
                        ]

digit6 = parseCharacter [ "  xxxxx "
                        , " xx     "
                        , "xx      "
                        , "xx      "
                        , "xxxxxxx "
                        , "xxx   xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xxxxxx "
                        ]

digit8 = parseCharacter [ " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xxxxxx "
                        ]

digit9 = parseCharacter [ " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx   xxx"
                        , " xxxxxxx"
                        , "      xx"
                        , "      xx"
                        , "     xx "
                        , " xxxxx  "
                        ]


-- All known characters
characters :: [(CharacterMatrix, Char)]
characters = [ (digit0, '0')
             , (digit1, '1')
             , (digit2, '2')
             , (digit6, '6')
             , (digit8, '8')
             , (digit9, '9')
             ]

diffChars :: CharacterMatrix -> CharacterMatrix -> Int
diffChars = diff `on` concat
    where diff a b = length $ filter id $ zipWith (/=) a b

matrixFromImage :: ImageCoord -> DynamicImage -> CharacterMatrix
matrixFromImage (left, top) (ImageRGB8 img) = [[(not . isBackground) (pixelAt img x y) | x <- [left..right]] | y <- [top..bottom]]
    where right = left + charWidth - 1
          bottom = top + charHeight - 1

charAtPrecise :: ImageCoord -> DynamicImage -> Maybe Char
charAtPrecise coords img = if maxFit < 5 then Just bestCharacter else Nothing
    where (maxFit, bestCharacter) = minimumBy (compare `on` fst) characterFits
          characterFits = map fitCharacter characters
          fitCharacter (example, char) = (diffChars actual example, char)
          actual = matrixFromImage coords img
