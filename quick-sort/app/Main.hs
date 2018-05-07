module Main where

import Lib

main :: IO ()
main = do
  let list = [1,5,7,9,3,5,4,7,2]
  putStrLn $ show list
  putStrLn $ show $ quickSort list

-- where first element of list is pivot
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = before ++ pivot ++ after
  where
    before = quickSort [x' | x' <- xs, x' <= x]
    pivot = [x]
    after = quickSort [x' | x' <- xs, x' > x]

--in order merge of 2 lists
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = quickSort x
merge [] y = quickSort y
merge x y = quickSort $ x ++ y

-- finds if a list is sorted or not
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:x':xs) = x <= x' && isSorted (x':xs)
