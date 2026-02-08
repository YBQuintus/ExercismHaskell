module IsbnVerifier (isbn) where

import Data.Maybe
import Data.Char

convert :: Bool -> Char -> Int
convert True 'X' = 10
convert _ n
  | isDigit n = read [n]
  | otherwise = -999


isbnFiltered :: String -> Bool
isbnFiltered (a:b:c:d:e:f:g:h:i:j:[]) = ((a' * 10) + (b' * 9) + (c' * 8) + (d' * 7) + (e' * 6) + (f' * 5) + (g' * 4) + (h' * 3) + (i' * 2) + j') `mod` 11 == 0
  where
      a' = convert False a
      b' = convert False b
      c' = convert False c
      d' = convert False d
      e' = convert False e
      f' = convert False f
      g' = convert False g
      h' = convert False h
      i' = convert False i
      j' = convert True j

isbnFiltered _ = False

isbn :: String -> Bool
isbn xs = isbnFiltered (filter (/= '-') xs)
