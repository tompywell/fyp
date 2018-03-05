module Main where

import Lib

main :: IO ()
main = do
  putStrLn $ show $ 5 `choose` 3
  someFunc

choose :: Int -> Int -> Int
choose n k = ([1,1]^n) !! k

instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

pascalAt :: Int -> Int -> Int
pascalAt _ 0 = 1
pascalAt n k | n==k = 1
pascalAt n k = (pascalAt (n-1) (k-1)) + (pascalAt (n-1) k)
--pascalAt n k = ((n-1) `choose` (k-1)) + ((n-1) `choose` k)
