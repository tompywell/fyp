module Main where

import Lib

main :: IO ()
main = do
  let tree = makeTree 5
  putStrLn $ show $ bf tree
  putStrLn $ show $ df tree

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

-- depth-first search
df :: Tree -> [Int]
df Empty = []
df (Node left val right) = df left ++ [val] ++ df right

-- bredth-first search
bf :: Tree -> [Int]
bf tree = bf' [tree]

-- helper
bf' :: [Tree] -> [Int]
bf' [] = []
bf' xs = map getVal xs ++ bf' (foo xs)

foo :: [Tree] -> [Tree]
foo trees = concat (map children trees)
  where
    children (Node l _ r) =
      (if l == Empty then [] else [l]) ++ (if r == Empty then [] else [r])

getVal :: Tree -> Int
getVal (Node _ v _) = v
