module Anagram (anagramsFor) where

import Data.Char
import Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagram xs) xss


isAnagram :: String -> String -> Bool
isAnagram target candidate = 
  let nt = normalise target
      nc = normalise candidate
  in nt == nc && map toLower target /= map toLower candidate

normalise :: String -> String
normalise = sort . map toLower