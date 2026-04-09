module Scrabble (scoreLetter, scoreWord) where

import Data.Char(toUpper)

scoreLetter :: Char -> Integer
scoreLetter 'a' = 1
scoreLetter 'A' = 1
scoreLetter 'E' = 1
scoreLetter 'I' = 1
scoreLetter 'O' = 1
scoreLetter 'U' = 1
scoreLetter 'L' = 1
scoreLetter 'N' = 1
scoreLetter 'R' = 1
scoreLetter 'S' = 1
scoreLetter 'T' = 1

scoreLetter 'D' = 2
scoreLetter 'G' = 2

scoreLetter 'B' = 3
scoreLetter 'C' = 3
scoreLetter 'M' = 3
scoreLetter 'P' = 3

scoreLetter 'F' = 4
scoreLetter 'H' = 4
scoreLetter 'V' = 4
scoreLetter 'W' = 4
scoreLetter 'Y' = 4

scoreLetter 'K' = 5

scoreLetter 'J' = 8
scoreLetter 'X' = 8

scoreLetter 'Q' = 10
scoreLetter 'Z' = 10

scoreLetter _ = 0

scoreWord :: String -> Integer
scoreWord = sum . map (scoreLetter . toUpper)
