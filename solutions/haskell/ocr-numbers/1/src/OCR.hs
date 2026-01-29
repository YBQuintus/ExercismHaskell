module OCR (convert) where

import Data.List

getFirstCharacter :: [String] -> [String]
getFirstCharacter xs = map (take 3) xs

getRestOfString :: [String] -> [String]
getRestOfString xs = map (drop 3) xs

recognizeCharacter :: [String] -> String
recognizeCharacter [ " _ "
                   , "| |"
                   , "|_|"
                   , "   " ] = "0"

recognizeCharacter [ "   "
                   , "  |"
                   , "  |"
                   , "   " ] = "1"

recognizeCharacter [ " _ "
                   , " _|"
                   , "|_ "
                   , "   "] = "2"

recognizeCharacter [ " _ "
                   , " _|"
                   , " _|"
                   , "   "] = "3"

recognizeCharacter [ "   "
                   , "|_|"
                   , "  |"
                   , "   "] = "4"

recognizeCharacter [ " _ "
                   , "|_ "
                   , " _|"
                   , "   "] = "5"

recognizeCharacter [ " _ "
                   , "|_ "
                   , "|_|"
                   , "   "] = "6"

recognizeCharacter [ " _ "
                   , "  |"
                   , "  |"
                   , "   "] = "7"

recognizeCharacter [ " _ "
                   , "|_|"
                   , "|_|"
                   , "   "] = "8"

recognizeCharacter [ " _ "
                   , "|_|"
                   , " _|"
                   , "   "] = "9"

recognizeCharacter [ " _ "
                   , "| |"
                   , "|_|"
                   , "   "] = "0"

recognizeCharacter xs
  | length xs == 4 && (length (head xs)) == 3 = "?"
  | otherwise = error "Incorrect text size"

recognizeString :: [String] -> String
recognizeString xs
  | all null xs = ""
  | otherwise = recognizeCharacter (getFirstCharacter xs) ++ recognizeString (getRestOfString xs)


recognizeLines :: [String] -> [String]
recognizeLines xs
  | length xs == 4 = [recognizeString xs]
  | (length xs `mod` 4) == 0 = [recognizeString (take 4 xs)] ++ (recognizeLines (drop 4 xs))
  
  
convert :: String -> String
convert xs = (intercalate "," (recognizeLines (lines xs)))

