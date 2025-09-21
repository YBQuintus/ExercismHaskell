module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows x = reverse $ rowsReversed x

rowsReversed :: Int -> [[Integer]]
rowsReversed 1 = [getRow 1]
rowsReversed x = (getRow x) : (rowsReversed (x-1))

getRow :: Int -> [Integer]
getRow 1 = [1]
getRow 2 = [1,1]
getRow x = [1] ++ map sumTuple (zip (getRow (x-1)) $ tail (getRow (x-1))) ++ [1]

sumTuple :: (Integer, Integer) -> Integer
sumTuple (x, y) = x + y
