module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just (collatzSteps n)

collatzSteps :: Integer -> Integer
collatzSteps 1 = 0
collatzSteps n = 1 + collatzSteps (step n)

step :: Integer -> Integer
step n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1
