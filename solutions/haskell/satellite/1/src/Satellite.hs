module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  | length preorder == 0 || length inorder == 0 = Nothing
  | length preorder /= length inorder = Nothing
  | sort preorder /= sort inorder = Nothing
  | length (nub preorder) /= length preorder || length (nub inorder) /= length inorder = Nothing
  | length preorder == 1 = Just (Branch Leaf (head preorder) Leaf)
  | otherwise = Just (Branch leftTree node rightTree)
    where
      node = head preorder
      Just inOrderIndex = findIndex (==node) inorder
      leftPreorderList = take inOrderIndex (tail preorder)
      rightPreorderList = drop (inOrderIndex) (tail preorder)
      leftInorderList = take inOrderIndex inorder
      rightInorderList = drop (inOrderIndex + 1) inorder
      Just leftTree = treeFromTraversals leftPreorderList leftInorderList
      Just rightTree = treeFromTraversals rightPreorderList rightInorderList