module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n 
  | n > 0 && n < 32 = handshakeRecursive n []
  | otherwise = []

handshakeRecursive :: Int -> [String] -> [String]
handshakeRecursive n xs
  | isWink n = handshakeRecursive (n-1) (xs ++ ["wink"])
  | isDoubleBlink n = handshakeRecursive (n-2) (xs ++ ["double blink"])
  | isCloseEyes n = handshakeRecursive (n-4) (xs ++ ["close your eyes"])
  | isJump n = handshakeRecursive (n-8) (xs ++ ["jump"])
  | isReverse n = handshakeRecursive (n-16) (reverse xs)
  | otherwise = xs


isWink :: Int -> Bool
isWink n = n `mod` 2 == 1

isDoubleBlink :: Int -> Bool
isDoubleBlink n = n `mod` 4 >= 2

isCloseEyes :: Int -> Bool
isCloseEyes n = n `mod` 8 >= 4

isJump :: Int -> Bool
isJump n = n `mod` 16 >= 8

isReverse :: Int -> Bool
isReverse n = n >= 16 && n < 32