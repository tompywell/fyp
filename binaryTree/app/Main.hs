module Main where

import Lib

main :: IO ()
main = someFunc

data Tree a = Nil | Node (Tree a) a (Tree a)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node left val right) x
  | x < val = Node (insert left x) val right
  | x == val = Node left val right
  | x > val = Node left val (insert right x)

delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete (Node left val right) x
  | x < val = Node (delete left x) val right
  | x == val = delete'  (Node left val right)
  | x > val = Node left val (delete right x)

delete' :: (Ord a) => Tree a -> Tree a
delete' (Node Nil val right) = right
delete' (Node left val Nil) = left
delete' (Node left val right) = (Node left val' right')
  where val' = leftMost right
        right' = delete right val'

leftMost :: (Ord a) => Tree a -> a
leftMost (Node Nil v _) = v
leftMost (Node left _ _) = leftMost left

toList :: (Ord a) => Tree a -> [a]
toList Nil = []
toList (Node left val right) =
  toList left ++
  [val] ++
  toList right

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Nil
fromList (x:xs) = fromList' (Node Nil x Nil) xs

fromList' :: (Ord a) => Tree a -> [a] -> Tree a
fromList' tr [] = tr
fromList' tr (x:xs) = fromList' (insert tr x) xs
