module Transpose (transpose) where

padLines :: [String] -> [String]
padLines [] = []
padLines (x:[]) = [x]
padLines (x:xs) = (x ++ replicate (maxLen - length x) ' '):(padLines xs)
  where
    maxLen = maximum (map length xs)

headDefaultString :: String -> String
headDefaultString "" = ""
headDefaultString x = [head x]

tailDefaultString :: String -> String
tailDefaultString "" = ""
tailDefaultString x = tail x

transposePadded :: [String] -> [String]
transposePadded [] = [] 
transposePadded lines
  | all null lines = []
  | otherwise = (concat (map headDefaultString lines)) : transposePadded (map tailDefaultString lines)

transpose :: [String] -> [String]
transpose lines = transposePadded (padLines lines)
