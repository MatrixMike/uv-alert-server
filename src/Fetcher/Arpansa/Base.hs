module Fetcher.Arpansa.Base where

type ImageCoord = (Int, Int)

-- Character dimensions
charWidth :: Int
charWidth = 8

charHeight :: Int
charHeight = 13

-- Stored characters for comparison, size charHeight x charWidth
type CharacterMatrix = [[Bool]]
