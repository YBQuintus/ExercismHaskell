module Queens (boardString, canAttack) where

import Data.List (intercalate)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString q1 q2 = unlines [ intercalate " " [cell (r, c) | c <- [0..7]] | r <- [0..7] ]
  where
    cell pos
      | Just pos == q1 = "W"
      | Just pos == q2 = "B"
      | otherwise      = "_"



canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (y1, x1) (y2, x2) =
    y1 == y2 || x1 == x2 || abs (y1 - y2) == abs (x1 - x2)

