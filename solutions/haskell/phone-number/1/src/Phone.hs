module Phone (number) where

number :: String -> Maybe String
number xs 
  | head (filterNumber xs) == '1' && length (filterNumber xs) == 11 && checkString (tail $ filterNumber xs) = Just (tail $ filterNumber xs)
  | length (filterNumber xs) == 10 && checkString (filterNumber xs) = Just (filterNumber xs)
  | otherwise = Nothing

isValidChar :: Char -> Bool
isValidChar x = elem x ['0'..'9']

filterNumber :: String -> String
filterNumber xs = filter isValidChar xs

checkString :: String -> Bool
checkString xs = elem (head xs) ['2'..'9'] && elem (xs!!3) ['2'..'9']
