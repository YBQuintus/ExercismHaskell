module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.Maybe

data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node _ (Node x t1 t2) _) = Just (Node x t1 t2)
bstLeft (Node _ Empty _) = (Just Empty)
bstLeft Empty = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ (Node x t1 t2)) = Just (Node x t1 t2)
bstRight (Node _ _ Empty) = (Just Empty)
bstRight Empty = Nothing

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node x _ _) = Just x

empty :: BST a
empty = Empty

fromListRecursive :: Ord a => [a] -> BST a -> BST a
fromListRecursive [x] t = insert x t
fromListRecursive (x:xs) t = fromListRecursive xs (insert x t)

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList xs = fromListRecursive xs Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = (Node x Empty Empty)
insert x (Node y t1 t2)
  | x <= y = (Node y (insert x t1) t2)
  | otherwise = (Node y t1 (insert x t2))

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node x t1 t2) = (toList t1) ++ [x] ++ (toList t2)
