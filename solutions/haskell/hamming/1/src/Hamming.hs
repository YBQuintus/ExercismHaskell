module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys | length xs == length ys = Just (addIfDifferent xs ys)
               | otherwise = Nothing

addIfDifferent :: String -> String -> Int
addIfDifferent xs ys | length xs == 0 = 0
                     | head xs == head ys = 0 + addIfDifferent (tail xs) (tail ys)
                     | otherwise = 1 + addIfDifferent (tail xs) (tail ys)
