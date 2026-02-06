module RunLength (decode, encode) where

import Data.Char

clampIfZero :: Int -> Int
clampIfZero 0 = 1
clampIfZero n = n

emptyIfOne :: Int -> String
emptyIfOne 1 = ""
emptyIfOne x = show x

decodeRecursive :: String -> Int -> String
decodeRecursive "" _ = ""
decodeRecursive (x:xs) counter
  | isDigit x = decodeRecursive xs (counter * 10 + (read [x]))
  | otherwise = take (clampIfZero counter) (repeat x) ++ decodeRecursive xs 0

decode :: String -> String
decode encodedText = decodeRecursive encodedText 0

encodeRecursive :: String -> Int -> Char -> String
encodeRecursive [] counter lastChar =
    emptyIfOne counter ++ [lastChar]

encodeRecursive (x:xs) counter lastChar
    | x == lastChar =
        encodeRecursive xs (counter + 1) lastChar
    | otherwise =
        emptyIfOne counter ++ [lastChar] ++
        encodeRecursive xs 1 x

encode :: String -> String
encode "" = ""
encode (char:text) = encodeRecursive text 1 char
