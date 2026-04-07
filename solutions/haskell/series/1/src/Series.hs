module Series (slices) where

import Data.Char(digitToInt)

slices :: Int -> String -> [[Int]]
slices 0 xs = replicate (length xs + 1) []
slices n xs
  | n > length xs = []
  | otherwise =
      map (map digitToInt) (windows n xs)

windows :: Int -> String -> [String]
windows n xs
  | length xs < n = []
  | otherwise = take n xs : windows n (tail xs)
