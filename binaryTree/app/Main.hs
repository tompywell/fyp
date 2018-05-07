module Main where

import Lib

main :: IO ()
main = someFunc

-- basic binary tree data type
data Tree = Nil | Node Tree Int Tree

-- insert  number into the tree
insert :: Tree -> Int -> Tree
insert Nil x = Node Nil x Nil
insert (Node left val right) x
  | x < val = Node (insert left x) val right
  | x == val = Node left val right
  | x > val = Node left val (insert right x)

-- delete a number form the tree
delete :: Tree -> Int -> Tree
delete Nil _ = Nil
delete (Node left val right) x
  | x < val = Node (delete left x) val right
  | x == val = delete'  (Node left val right)
  | x > val = Node left val (delete right x)

-- a helper function for delete
delete' :: Tree -> Tree
delete' (Node Nil val right) = right
delete' (Node left val Nil) = left
delete' (Node left val right) = (Node left val' right')
  where val' = leftMost right
        right' = delete right val'

-- finds the leftmost value in a tree, usually a subtree
leftMost :: Tree -> Int
leftMost (Node Nil v _) = v
leftMost (Node left _ _) = leftMost left

--recursively turns a tree into a list of values
toList :: Tree -> [Int]
toList Nil = []
toList (Node left val right) =
  toList left ++
  [val] ++
  toList right

-- inserts a list of numbesr into a tree
fromList :: [Int] -> Tree
fromList [] = Nil
fromList (x:xs) = fromList' (Node Nil x Nil) xs

-- a helper function for fromList
fromList' :: Tree -> [Int] -> Tree
fromList' tr [] = tr
fromList' tr (x:xs) = fromList' (insert tr x) xs
