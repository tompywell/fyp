module Main where

import Lib

main :: IO ()
main = someFunc

-- assuming the tree is 'full' to a certain depth
inOrderDepths :: Int -> [Int]
inOrderDepths 0 = []
inOrderDepths n = inOrderDepths (n-1) ++ [n] ++ inOrderDepths (n-1)
