module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

allergies :: Int -> [Allergen]
allergies score =
  [ allergen
  | allergen <- [minBound .. maxBound]
  , testBit score (fromEnum allergen)
  ]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
  testBit score (fromEnum allergen)