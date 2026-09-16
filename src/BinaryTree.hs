module BinaryTree where

data Tree a = Empty | Node a (Tree a) (Tree a)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x t@(Node y l r) = case compare x y of
    EQ -> t
    LT -> Node y (insert x l) r
    GT -> Node y l (insert x r)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Empty

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)