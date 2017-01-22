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

maybeSplitHead :: [a] -> Maybe (a, [a])
maybeSplitHead [] = Nothing
maybeSplitHead (a:as) = Just (a, as)

extrapolate :: Fractional a => (a, a) -> (a, a) -> a -> a
extrapolate (a1, b1) (a2, b2) a = b1 + (b2 - b1) * (a - a1) / (a2 - a1)

removeAccents :: T.Text -> T.Text
removeAccents = T.filter (not . isMark) . normalize NFD

-- Replace characters not allowed in Pebble pin IDs and topic names
normalizeValue :: T.Text -> T.Text
normalizeValue = T.replace " " "_" . removeAccents

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs
