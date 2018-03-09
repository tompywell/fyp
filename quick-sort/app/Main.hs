module Main where

import Lib

main :: IO ()
main = someFunc

-- where first element of list is pivot
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = before ++ pivot ++ after
  where
    before = quickSort [x' | x' <- xs, x' <= x]
    pivot = [x]
    after = quickSort [x' | x' <- xs, x' > x]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = quickSort x
merge [] y = quickSort y
merge x y = quickSort $ x ++ y

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:x':xs) = x <= x' && isSorted (x':xs)
