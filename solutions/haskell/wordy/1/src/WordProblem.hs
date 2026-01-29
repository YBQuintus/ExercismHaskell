module WordProblem (answer) where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read
import Text.ParserCombinators.ReadPrec

convert :: [String] -> [String]
convert [] = []
convert [x] = [x]
convert (x:"by":xs) = (x ++ "by") : convert xs
convert (x:xs) = x : convert xs

validOperator :: Integral a => String -> Maybe (a -> a -> a)
validOperator "plus"         = Just (+)
validOperator "minus"        = Just (-)
validOperator "multipliedby" = Just (*)
validOperator "dividedby" = Just (div)
validOperator _              = Nothing

evaluateSequence :: Integral a => Maybe a-> Maybe (a -> a -> a) -> Maybe a -> Maybe a
evaluateSequence (Just n1) (Just operator) (Just n2) = (Just (n1 `operator` n2))
evaluateSequence _ _ _ = Nothing

evaluate :: [String] -> Maybe Integer
evaluate [] = Nothing
evaluate (x:[]) = readMaybe x
evaluate (x:y:[]) = Nothing
evaluate (x:y:z:[]) = evaluateSequence (readMaybe x) (validOperator y) (readMaybe z)
evaluate (x:y:z:xs) =
  case evaluateSequence (readMaybe x) (validOperator y) (readMaybe z) of
    Just n  -> evaluate (show n : xs)
    Nothing -> Nothing
    

answer :: String -> Maybe Integer
answer problem
  | not (take 8 problem == "What is ") = Nothing
  | otherwise = evaluate (convert (splitOn " " (drop 8 (init problem))))
