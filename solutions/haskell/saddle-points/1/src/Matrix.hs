module Matrix (saddlePoints) where

import Data.Array (Array)
import Data.Array.IArray

getColumn :: Array (Int, Int) Int -> Int -> [Int]
getColumn matrix col = [matrix ! (r, col) | r <- [rMin .. rMax]] where ((rMin, _), (rMax, _)) = bounds matrix

getRow :: Array (Int, Int) Int -> Int -> [Int]
getRow matrix row = [matrix ! (row, c) | c <- [cMin .. cMax]] where ((_, cMin), (_, cMax)) = bounds matrix

largestInRow :: Array (Int, Int) Int -> (Int, Int) -> Bool
largestInRow matrix (row, col) = matrix ! (row, col) == maximum (getRow matrix row)

smallestInCol :: Array (Int, Int) Int -> (Int, Int) -> Bool
smallestInCol matrix (row, col) = matrix ! (row, col) == minimum (getColumn matrix col)

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = [ix | ix <- indices matrix, largestInRow matrix ix, smallestInCol matrix ix]
