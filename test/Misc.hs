module Misc where

between :: Ord a => a -> a -> a -> Bool
between low high value = value > low && value < high
