module CryptoSquare (encode) where

import Data.Char (toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords $ transpose paddedRows
  where
    (r, c) = getRectangleSize (length (normalise xs))
    rows = splitIntoRows c (normalise xs)
    paddedRows = map (\x -> x ++ replicate (c - length x) ' ') rows

normalise :: String -> String
normalise x = filter (`elem` (['a'..'z'] ++ ['0'..'9']) ) (map toLower x)

getRectangleSize :: Int -> (Int, Int)
getRectangleSize x = iterateRectangleSize x 1

iterateRectangleSize :: Int -> Int -> (Int, Int)
iterateRectangleSize length tempr
  | tempr * tempr >= length = (tempr, tempr)
  | tempr * (tempr + 1) >= length = (tempr, tempr + 1)
  | otherwise = iterateRectangleSize length (tempr + 1)

splitIntoRows :: Int -> String -> [String]
splitIntoRows _ [] = []
splitIntoRows n s = take n s : splitIntoRows n (drop n s)
