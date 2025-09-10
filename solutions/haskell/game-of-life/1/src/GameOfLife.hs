module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick xss = [[tickCell xss x y | x <- [0..length (xss !! y) - 1]] | y <- [0..length xss - 1]]

tickCell :: [[Int]] -> Int -> Int -> Int
tickCell xss x y
  | getCellData xss x y == 1 = if (getSurroundData xss x y == 2 || getSurroundData xss x y == 3) then 1 else 0
  | getCellData xss x y == 0 = if (getSurroundData xss x y == 3) then 1 else 0
  | otherwise = 0
  
getSurroundData :: [[Int]] -> Int -> Int -> Int
getSurroundData xss x y = getCellData xss (x-1) (y-1) + getCellData xss (x-1) y + getCellData xss (x-1) (y+1) + getCellData xss x (y-1) + getCellData xss x (y+1) + getCellData xss (x+1) (y-1) + getCellData xss (x+1) y + getCellData xss (x+1) (y+1)

getCellData :: [[Int]] -> Int -> Int -> Int
getCellData xss x y
  | isValidCell xss x y = xss!!y!!x
  | otherwise = 0
  
isValidCell :: [[Int]] -> Int -> Int -> Bool
isValidCell xss x y
  | y >= 0 && y < length xss = x >= 0 && x < length(xss!!y)
  | otherwise = False
