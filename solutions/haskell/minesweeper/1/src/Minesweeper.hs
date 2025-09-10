module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = [[countMines board x y | x <- [0 .. length (board !! y) - 1]] | y <- [0 .. length board - 1]]

isMine :: Char -> Bool
isMine '*' = True
isMine _   = False

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

validPos :: [String] -> Int -> Int -> Bool
validPos board x y = y >= 0 && y < length board && x >= 0 && x < length (board !! y)

isMineAtPosition :: [String] -> Int -> Int -> Int
isMineAtPosition board x y
  | validPos board x y = boolToInt $ isMine (board !! y !! x)
  | otherwise          = 0

countMinesAtPosition :: [String] -> Int -> Int -> Int
countMinesAtPosition board x y = sum [isMineAtPosition board (x+dx) (y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

convertCountToChar :: Int -> Char
convertCountToChar 0 = ' '
convertCountToChar x = head $ show x

countMines :: [String] -> Int -> Int -> Char
countMines board x y
  | isMineAtPosition board x y == 1 = '*'
  | otherwise = convertCountToChar $ countMinesAtPosition board x y
