module WordCount (wordCount) where

import Data.List (sort, group)
import Data.Char (isAlphaNum, toLower)


wordCount :: String -> [(String, Int)]
wordCount xs = map (\ws -> (head ws, length ws)) groupedWords
  where
    cleanedWords = words $ map (\c -> if isAlphaNum c || c == ' ' || c == '\'' then toLower c else ' ') xs
    cleanedNoQuotes = map removeQuotesFromWord cleanedWords
    groupedWords = group $ sort cleanedNoQuotes
    removeQuotesFromWord :: String -> String
    removeQuotesFromWord word
      | length word >= 2 && head word == '\'' && last word == '\'' = init (tail word)
      | otherwise = word


    
