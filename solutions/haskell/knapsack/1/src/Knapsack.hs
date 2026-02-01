module Knapsack (maximumValue) where

import Data.Array

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dp ! (numItems, capacity)
  where 
    numItems = length items
    itemArray = listArray (1, numItems) items

    dp :: Array (Int, Int) Int
    dp = array bounds [((i, w), bestValue i w) | i <- [0..numItems], w <- [0..capacity]]

    bounds = ((0,0), (numItems, capacity))
    bestValue 0 _ = 0

    bestValue i w =
      let (weight, value) = itemArray ! i
      in if weight > w
         then dp ! (i-1, w)
         else max (dp ! (i-1, w))
                  (value + dp ! (i-1, w - weight))
