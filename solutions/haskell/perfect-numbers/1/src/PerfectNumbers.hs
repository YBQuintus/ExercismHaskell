module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
  | n <= 0 = Nothing
  | sum (generateFactors n) == n = Just Perfect
  | sum (generateFactors n) > n = Just Abundant
  | sum (generateFactors n) < n = Just Deficient
  | otherwise = Nothing

generateFactors :: Int -> [Int]
generateFactors n = [x | x <- [1..(n-1)] , n `mod` x == 0]
