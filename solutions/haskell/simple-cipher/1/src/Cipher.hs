module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (ord, chr)

getNum :: Char -> Int
getNum c
  | c >= 'a' && c <= 'z' = ord c - ord 'a'
  | c >= 'A' && c <= 'Z' = ord c - ord 'A'
  | otherwise = 0

shiftChar :: Int -> Char -> Char
shiftChar offset c
  | c >= 'a' && c <= 'z' = chr (((ord c - ord 'a' + offset) `mod` 26) + ord 'a')
  | c >= 'A' && c <= 'Z' = chr (((ord c - ord 'A' + offset) `mod` 26) + ord 'A')
  | otherwise            = c

caesarEncode :: String -> String -> String
caesarEncode key text = zipWith (\k t -> shiftChar (getNum k) t) (cycle key) text

caesarDecode :: String -> String -> String
caesarDecode key text = zipWith (\k t -> shiftChar (26 - (getNum k)) t) (cycle key) text

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- mapM randomChar text
    let encrypted = caesarEncode key text
    return (key, encrypted)

randomChar :: Char -> IO Char
randomChar _ = randomRIO ('a','z')
