module Yacht (yacht, Category(..)) where

import Data.List (sort, group)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

fullHouse :: [Int] -> Int
fullHouse dice =
  let groups = sort (map length (group (sort dice)))
  in if groups == [2,3]
        then sum dice
        else 0



yacht :: Category -> [Int] -> Int
yacht Ones dice = sum (filter (== 1) dice)
yacht Twos dice = sum (filter (== 2) dice)
yacht Threes dice = sum (filter (== 3) dice)
yacht Fours dice = sum (filter (== 4) dice)
yacht Fives dice = sum (filter (== 5) dice)
yacht Sixes dice = sum (filter (== 6) dice)
yacht FullHouse dice = fullHouse dice
yacht FourOfAKind dice =
  case filter ((>=4) . length) (group (sort dice)) of
    (xs:_) -> 4 * head xs
    _      -> 0
yacht LittleStraight dice =
  if sort dice == [1,2,3,4,5] then 30 else 0
yacht BigStraight dice =
  if sort dice == [2,3,4,5,6] then 30 else 0
yacht Choice dice = sum dice
yacht Yacht dice =
  if length (group dice) == 1 then 50 else 0