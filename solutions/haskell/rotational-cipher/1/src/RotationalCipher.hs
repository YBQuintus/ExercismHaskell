module RotationalCipher (rotate) where

import Data.Char
import Data.List

alphabetStringLower :: String
alphabetStringLower = "abcdefghijklmnopqrstuvwxyz"

alphabetStringHigher :: String
alphabetStringHigher = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

rotateChar :: Int -> Char -> Char
rotateChar key x
  | isLower x = case elemIndex x alphabetStringLower of
                  Just n -> alphabetStringLower !! ((n+key) `mod` 26)
                  Nothing -> x
  | otherwise = case elemIndex x alphabetStringHigher of
                  Just n -> alphabetStringHigher !! ((n+key) `mod` 26)
                  Nothing -> x


rotate :: Int -> String -> String
rotate _ "" = ""
rotate key (x:xs) = (rotateChar key x):(rotate key xs)
