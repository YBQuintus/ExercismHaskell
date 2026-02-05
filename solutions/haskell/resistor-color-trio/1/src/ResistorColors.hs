module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

convert :: Color -> Int
convert Black = 0
convert Brown = 1
convert Red = 2
convert Orange = 3
convert Yellow = 4
convert Green = 5
convert Blue = 6
convert Violet = 7
convert Grey = 8
convert White = 9

label :: Resistor -> String
label resistor
  | result >= 1000000000 = show (result `div` 1000000000) ++ " gigaohms"
  | result >= 1000000 = show (result `div` 1000000) ++ " megaohms"
  | result >= 1000 = show (result `div` 1000) ++ " kiloohms"
  | otherwise = show (result) ++ " ohms"
    where
      result = ohms resistor

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = ((10 * convert c1) + convert c2) * 10 ^ (convert c3)
