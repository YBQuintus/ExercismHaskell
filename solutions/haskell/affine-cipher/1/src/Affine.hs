module Affine (decode, encode) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

alphabetString:: String
alphabetString = "abcdefghijklmnopqrstuvwxyz"

egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (a, 1, 0)
egcd a b =
  let (g, x1, y1) = egcd b (a `mod` b)
  in (g, y1, x1 - (a `div` b) * y1)

modInv :: Int -> Int -> Int
modInv a m =
  let (g, x, _) = egcd a m
  in if g /= 1
       then error "modInv: inverse does not exist"
       else x `mod` m

decodeFunction :: (Int, Int) -> Char -> String
decodeFunction (a,b) x = case elemIndex (toLower x) alphabetString of
                          Just y -> [alphabetString !! ((modInv a 26)*(y-b) `mod` 26)]
                          Nothing -> if isDigit x
                                       then [x]
                                     else ""

encodeFunction :: (Int, Int) -> Char -> String
encodeFunction (a,b) x = case elemIndex (toLower x) alphabetString of
                          Just i -> [alphabetString !! ((a*i + b) `mod` 26)]
                          Nothing -> if isDigit x 
                                       then [x]
                                     else ""

decode :: (Int, Int) -> String -> Maybe String
decode _ "" = Just ""
decode (a,b) (x:xs)
  | a `mod` 2 == 0 || a `mod` 13 == 0 = Nothing
  | otherwise = case decode (a,b) xs of
                  Just ys -> Just ((decodeFunction (a,b) x) ++ ys)
                  Nothing -> Nothing

encodeIntermediate :: (Int, Int) -> String -> Maybe String
encodeIntermediate _ "" = Just ""
encodeIntermediate (a,b) (x:xs)
  | a `mod` 2 == 0 || a `mod` 13 == 0 = Nothing
  | otherwise = case encodeIntermediate (a,b) xs of
                  Just ys -> Just ((encodeFunction (a,b) x) ++ ys)
                  Nothing -> Nothing

encode :: (Int, Int) -> String -> Maybe String
encode (a,b) xs = case encodeIntermediate (a,b) xs of
                    Just ys -> Just (intercalate " " (chunksOf 5 ys))
                    Nothing -> Nothing
