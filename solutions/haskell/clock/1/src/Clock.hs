module Clock (addDelta, fromHourMin, toString) where

import Prelude

data Clock = Analog Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Analog hour' min'
  where 
    totalMinutes = hour * 60 + min
    hour' = (totalMinutes `div` 60) `mod` 24
    min' = totalMinutes `mod` 60

toString :: Clock -> String
toString (Analog x y) = padTime x ++ ":" ++ padTime y

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Analog x y) = fromHourMin (x + hour) (y + min)

padTime :: Int -> String
padTime x
  | length (show x) == 1 = '0' : (show x)
  | otherwise = show x