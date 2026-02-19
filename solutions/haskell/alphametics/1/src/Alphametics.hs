module Alphametics (solve) where

import Data.Char (isUpper)
import Data.List (nub)
import qualified Data.Map.Strict as M

type Assignment = M.Map Char Int

solve :: String -> Maybe [(Char, Int)]
solve input = do
    (lhs, rhs) <- parseEquation input
    let letters = nub (concat (rhs:lhs))

    if length letters > 10
       then Nothing
       else fmap M.toList $
            search letters M.empty [0..9] lhs rhs

search
  :: [Char]       
  -> Assignment   
  -> [Int]         
  -> [String]     
  -> String      
  -> Maybe Assignment

search [] assign _ lhs rhs =
    if validSolution assign lhs rhs
       then Just assign
       else Nothing

search (c:cs) assign digits lhs rhs =
    tryDigits digits
  where
    tryDigits [] = Nothing
    tryDigits (d:ds)
      | invalidLeadingZero c d (lhs ++ [rhs]) =
          tryDigits ds
      | otherwise =
          case search cs (M.insert c d assign) (remove d digits) lhs rhs of
              Just sol -> Just sol
              Nothing  -> tryDigits ds

validSolution :: Assignment -> [String] -> String -> Bool
validSolution assign lhs rhs =
    sum (map (wordValue assign) lhs) == wordValue assign rhs

wordValue :: Assignment -> String -> Int
wordValue assign =
    foldl (\n c -> n*10 + digit c) 0
  where
    digit c = assign M.! c

invalidLeadingZero :: Char -> Int -> [String] -> Bool
invalidLeadingZero c d wordsList =
    d == 0 && any (\w -> not (null w) && head w == c) wordsList

remove :: Eq a => a -> [a] -> [a]
remove x = filter (/= x)

parseEquation :: String -> Maybe ([String], String)
parseEquation input =
    case words input of
      ws ->
        let ups = filter (all isUpper) ws
        in case break (== "==") ws of
            (lhs, "==":rhs:_) ->
                Just (filter (all isUpper) lhs, rhs)
            _ -> Nothing
