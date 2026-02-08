module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Node a (LinkedList a) | NodeEnd deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Node x _) = x

fromList :: [a] -> LinkedList a
fromList [] = NodeEnd
fromList [x] = (Node x NodeEnd)
fromList (x:xs) = (Node x (fromList xs))

isNil :: LinkedList a -> Bool
isNil NodeEnd = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = (Node x linkedList)

next :: LinkedList a -> LinkedList a
next (Node x xs) = xs

nil :: LinkedList a
nil = NodeEnd

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = fromList (reverse (toList linkedList))

toList :: LinkedList a -> [a]
toList NodeEnd = []
toList (Node x NodeEnd) = [x]
toList (Node x xs) = x:(toList xs)
