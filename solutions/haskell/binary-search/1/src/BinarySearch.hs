module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = go low high
  where
    (low, high) = bounds array

    go l h
      | l > h     = Nothing
      | otherwise =
          case compare x midVal of
            LT -> go l (mid - 1)
            GT -> go (mid + 1) h
            EQ -> Just mid
      where
        mid = l + (h - l) `div` 2
        midVal = array ! mid
