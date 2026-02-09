module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

import Data.List (sort, nub)

data CustomSet a = Set [a] deriving (Eq, Show, Ord)

normalise :: Ord a => [a] -> [a]
normalise = nub . sort

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete x (Set xs) = (Set (filter (/= x) xs))

difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference (Set xs) setB =
  Set (filter (not . memberReverse setB) xs)

empty :: Ord a => CustomSet a
empty = (Set []) 

fromListRecursive :: Ord a => [a] -> CustomSet a -> CustomSet a
fromListRecursive [] xs = xs
fromListRecursive (x:xs) (Set ys)
  | elem x ys = fromListRecursive xs (Set ys)
  | otherwise = fromListRecursive xs (Set (normalise (x:ys)))

fromList :: Ord a => [a] -> CustomSet a
fromList xs = fromListRecursive xs (Set [])

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x (Set xs)
  | elem x xs = (Set xs)
  | otherwise = (Set (normalise (x:xs)))

insertList :: Ord a => [a] -> CustomSet a -> CustomSet a
insertList [] ys = ys
insertList (x:xs) ys = (insertList xs (insert x ys))

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection (Set xs) (Set ys) = (Set (filter (memberReverse (Set ys)) xs))

isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = (length setIntersection) == 0
  where (Set setIntersection) = intersection setA setB

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (Set xs) (Set ys) = setIntersection == xs
  where setIntersection = filter (memberReverse (Set ys)) xs

member :: Ord a => a -> CustomSet a -> Bool
member x (Set xs) = elem x xs

memberReverse :: Ord a => CustomSet a -> a -> Bool
memberReverse (Set xs) x = member x (Set xs)

null :: Ord a => CustomSet a -> Bool
null (Set []) = True
null _ = False

size :: Ord a => CustomSet a -> Int
size (Set xs) = length xs

toList :: Ord a => CustomSet a -> [a]
toList (Set xs) = xs

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union (Set xs) (Set ys) = insertList xs (Set ys)
