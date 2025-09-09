module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate xs = concatMap (takeInitial) (words (separateCamelCase $ sanitiseString xs))

takeInitial :: String -> String
takeInitial [] = ""
takeInitial (x:_) = [toUpper x]

sanitise :: Char -> Bool
sanitise x
  | elem (toUpper x) ['A'..'Z'] = True
  | elem x [' ', '-'] = True
  | otherwise = False

replaceHyphen :: Char -> Char
replaceHyphen x
  | x == '-' = ' '
  | otherwise = x

separateCamelCase :: String -> String
separateCamelCase [] = ""
separateCamelCase [x] = [x]
separateCamelCase (x:y:xs)
  | (toLower x) == x && (toUpper y) == y = x : ' ' : y : (separateCamelCase xs)
  | otherwise = x:y:(separateCamelCase xs)
  
sanitiseString :: String -> String
sanitiseString x = map replaceHyphen (filter sanitise x)