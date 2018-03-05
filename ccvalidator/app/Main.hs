module Main where

import Lib

main :: IO ()
main = do
  let start = 1000000000000000
  let end = 9999999999999999
  putStrLn $ show $ filter validate [start..end]

validate :: Integer -> Bool
validate x = mod (sumDigits (double (toDigits x))) 10 == 0

toDigits :: Integer -> [Integer]
toDigits x
  | x < 10 = [x]
  | otherwise = (toDigits (quot x 10)) ++ [mod x 10]

double :: [Integer] -> [Integer]
double [] = []
double (x:x':xs) = [x*2, x'] ++ double xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumSimple x) + (sumDigits xs)

sumSimple :: Integer -> Integer
sumSimple x = foldl (+) (0) (toDigits x)
