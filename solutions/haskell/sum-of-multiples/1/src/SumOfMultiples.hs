module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = foldl (+) 0 (getFactorsList factors limit)

getFactorsList :: [Integer] -> Integer -> [Integer]
getFactorsList (x:[]) limit = listOfMultiplesLessThan x limit
getFactorsList (x:xs) limit = combine (listOfMultiplesLessThan x limit) (getFactorsList xs limit)

listOfMultiplesLessThan :: Integer -> Integer -> [Integer]
listOfMultiplesLessThan factor limit
  | factor == 0 = []
  | limit `mod` factor == 0 = map (*factor) [1..(limit `div` factor - 1)]
  | otherwise = map (*factor) [1..(limit `div` factor)]

combine :: Eq a => [a] -> [a] -> [a]
combine x y = nub (x ++ y)