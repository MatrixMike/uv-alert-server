{-# Language OverloadedStrings #-}
module Utils where

import Data.Char
import qualified Data.Text as T
import Data.Text.ICU.Normalize

readEither :: Read a => e -> String -> Either e a
readEither err str = case reads str of
    [(res, "")] -> Right res
    _ -> Left err

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

-- | Extrapolate assuming a linear dependency.
extrapolate
    :: (Fractional b, Real b, Fractional a)
    => (a, b) -> (a, b) -> b -> a
extrapolate (a1, b1) (a2, b2) b = a1 + (a2 - a1) * percentage
    where
        percentage = fromRational $ toRational $ (b - b1) / (b2 - b1)

-- | Find the first value where the corresponding value is greater than the given one.
findValueMonotonic
    :: (Fractional a, Real a, Fractional b)
    => a -> [(b, a)] -> Maybe b
findValueMonotonic _ [] = Nothing
findValueMonotonic _ [_] = Nothing
findValueMonotonic a (p1:ps@(p2:_))
    | snd p1 >= a = Just $ fst p1
    | snd p2 >= a = Just $ extrapolate p1 p2 a
    | otherwise = findValueMonotonic a ps

removeAccents :: T.Text -> T.Text
removeAccents = T.filter (not . isMark) . normalize NFD

-- Replace characters not allowed in Pebble pin IDs and topic names
normalizeValue :: T.Text -> T.Text
normalizeValue = T.replace " " "_" . removeAccents

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs
