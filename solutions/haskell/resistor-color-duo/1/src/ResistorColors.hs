module ResistorColors (Color(..), value) where

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
  deriving (Eq, Show, Enum, Bounded)

bandEncode :: Color -> Int
bandEncode Black = 0
bandEncode Brown = 1
bandEncode Red = 2
bandEncode Orange = 3
bandEncode Yellow = 4
bandEncode Green = 5
bandEncode Blue = 6
bandEncode Violet = 7
bandEncode Grey = 8
bandEncode White = 9

value :: (Color, Color) -> Int
value (a, b) = 10 * (bandEncode a) + bandEncode b
