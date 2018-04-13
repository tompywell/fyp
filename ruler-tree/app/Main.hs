module Main where

import Lib

main :: IO ()
main = someFunc

-- assuming the tree is 'full' to a certain depth
inOrderDepths :: Int -> [Int]
inOrderDepths 0 = []
inOrderDepths n = inOrderDepths (n-1) ++ [n] ++ inOrderDepths (n-1)

data Tree = Empty
          | Node Tree Int Tree
  deriving Eq

-- builds a full tree to a certain depth
makeTree :: Int -> Tree
makeTree 1 = Node Empty 1 Empty
makeTree n = Node (makeTree (n-1)) n (makeTree (n-1))

df :: Tree -> [Int]
df Empty = []
df (Node left val right) = df left ++ [val] ++ df right

bf :: Tree -> [Int]
bf tree = bf' [tree]

bf' :: [Tree] -> [Int]
bf' [] = []
bf' xs = map getVal xs ++ bf' (poon xs)

poon :: [Tree] -> [Tree]
poon trees = concat (map children trees)
  where
    children (Node l _ r) =
      (if l == Empty then [] else [l]) ++ (if r == Empty then [] else [r])

getVal :: Tree -> Int
getVal (Node _ v _) = v
