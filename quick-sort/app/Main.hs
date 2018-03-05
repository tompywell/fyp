module Main where

import Lib

main :: IO ()
main = someFunc

-- where first element of list is pivot
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  quickSort [x' | x' <- xs, x' <= x] ++
  [x] ++
  quickSort [x' | x' <- xs, x' > x]
