module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List.Split

data Matrix a = Matrix [[a]] deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix matrix) = length (head matrix)

column :: Int -> Matrix a -> Vector a
column x (Matrix matrix) = V.fromList (map (!! (x-1)) matrix)

flatten :: Matrix a -> Vector a
flatten (Matrix matrix) = V.fromList (concat matrix)

fromList :: [[a]] -> Matrix a
fromList xss = (Matrix xss)

fromString :: Read a => String -> Matrix a
fromString xs =
  Matrix $ map (map read . words) (lines xs)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (row, col) matrix = (Matrix (chunksOf col vector))
  where
    vector = V.toList (flatten matrix)

row :: Int -> Matrix a -> Vector a
row x (Matrix matrix) = V.fromList (matrix !! (x-1))

rows :: Matrix a -> Int
rows (Matrix matrix) = length matrix

shape :: Matrix a -> (Int, Int)
shape (Matrix []) = (0,0)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose (Matrix ([]:_)) = (Matrix [])
transpose (Matrix matrix) = (Matrix ((map head matrix) : tailMatrix))
  where
    Matrix tailMatrix = transpose (Matrix (map tail matrix))
