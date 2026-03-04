module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (foldl')

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise =
      case foldl' step Nothing pairs of
        Nothing -> Nothing
        Just (p, fs) -> Just (p, reverse fs)
  where
    pairs =
      [ (a, b)
      | a <- [minFactor .. maxFactor]
      , b <- [a .. maxFactor]
      ]

    step acc (a, b) =
      let p = a * b
      in if not (isPalindrome p)
         then acc
         else case acc of
           Nothing -> Just (p, [(a, b)])
           Just (best, fs)
             | p > best  -> Just (p, [(a, b)])
             | p == best -> Just (best, (a, b) : fs)
             | otherwise -> acc


smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise =
      case foldl' step Nothing pairs of
        Nothing -> Nothing
        Just (p, fs) -> Just (p, reverse fs)
  where
    pairs =
      [ (a, b)
      | a <- [minFactor .. maxFactor]
      , b <- [a .. maxFactor]
      ]

    step acc (a, b) =
      let p = a * b
      in if not (isPalindrome p)
         then acc
         else case acc of
           Nothing -> Just (p, [(a, b)])
           Just (best, fs)
             | p < best  -> Just (p, [(a, b)])
             | p == best -> Just (best, (a, b) : fs)
             | otherwise -> acc


isPalindrome :: Integer -> Bool
isPalindrome n =
  let s = show n
  in s == reverse s