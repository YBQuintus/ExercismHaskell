module Prime (nth) where

import Prelude

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just ((filter isPrime [2..999999])!!(n-1))

isPrime :: Integer -> Bool
isPrime n 
  | n < 2 = False
  | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]
