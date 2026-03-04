module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2..n]
  where
    sieve []     = []
    sieve (p:xs) =
      p : sieve (removeMultiples p xs)

    removeMultiples _ [] = []
    removeMultiples p xs =
      filter (`notElem` multiples) xs
      where
        multiples = takeWhile (<= n) [p*p, p*p+p ..]