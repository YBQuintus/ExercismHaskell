module Luhn (isValid) where

import Prelude
import Data.Char

isValid :: String -> Bool
isValid n 
  | validString n = sum (applyToEveryOtherDigit doubleClampNine (map digitToInt(reverse (filter (isDigit) n)))) `mod` 10 == 0
  | otherwise = False

applyToEveryOtherDigit :: (Int -> Int) -> [Int] -> [Int]
applyToEveryOtherDigit f xs = [if i `mod` 2 == 1 then f d else d | (i,d) <- zip [0..] xs]

doubleClampNine :: Int -> Int
doubleClampNine 9 = 9
doubleClampNine x = let d = x * 2 in if d > 9 then d - 9 else d

removeSpaces :: String -> String
removeSpaces = filter (`elem` ['0'..'9'])

validString :: String -> Bool
validString xs = length (removeSpaces xs) > 1 && foldr (&&) True (map (`elem` (' ':['0'..'9'])) xs)