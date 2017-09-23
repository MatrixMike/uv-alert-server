{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Char
import qualified Data.Text as T
import Data.Text.ICU.Normalize

readEither :: Read a => e -> String -> Either e a
readEither err str =
  case reads str of
    [(res, "")] -> Right res
    _ -> Left err

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

-- | Extrapolate assuming a linear dependency.
extrapolate ::
     (Fractional a, Real a, Fractional b) => (b, a) -> (b, a) -> a -> b
extrapolate (b1, a1) (b2, a2) a = b1 + (b2 - b1) * percentage
  where
    percentage = fromRational $ toRational $ (a - a1) / (a2 - a1)

-- | Extrapolate inside the interval only.
extrapolateClipped ::
     (Fractional a, Real a, Fractional b) => (b, a) -> (b, a) -> a -> b
extrapolateClipped p1@(b1, a1) p2@(b2, a2) a
  | (a <= a1) && (a1 <= a2) = b1
  | (a <= a2) && (a2 <= a1) = b2
  | (a >= a1) && (a1 >= a2) = b1
  | (a >= a2) && (a2 >= a1) = b2
  | otherwise = extrapolate p1 p2 a

-- | Find the first value where the corresponding value is greater or equal than the given one.
findValueMonotonic ::
     (Fractional a, Real a, Fractional b) => a -> [(b, a)] -> Maybe b
findValueMonotonic a ps =
  case findIntervals a ps of
    [] -> Nothing
    (b, _):_ -> Just b

findIntervals ::
     (Fractional a, Real a, Fractional b) => a -> [(b, a)] -> [(b, b)]
findIntervals a ps =
  case break good ps of
    (_, []) -> []
    (low, rest) ->
      let (high, nextRound) = break bad rest
          lastLow =
            case low of
              [] -> head high
              _ -> last low
          firstLow =
            case nextRound of
              [] -> last high
              _ -> head nextRound
          b1 = extrapolateClipped lastLow (head high) a
          b2 = extrapolateClipped (last high) firstLow a
      in (b1, b2) : findIntervals a nextRound
  where
    good (_, v) = v >= a
    bad = not . good

removeAccents :: T.Text -> T.Text
removeAccents = T.filter (not . isMark) . normalize NFD

-- Replace characters not allowed in Pebble pin IDs and topic names
normalizeValue :: T.Text -> T.Text
normalizeValue = T.map removeInvalidChars . removeAccents
  where
    removeInvalidChars ' ' = '_'
    removeInvalidChars '\'' = '_'
    removeInvalidChars c = c

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs
