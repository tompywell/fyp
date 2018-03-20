module Main where

import Lib

main :: IO ()
main = someFunc

phi :: Float
phi = (1 + sqrt 5) / 2

fib :: Integer -> Integer
fib n = round((phi ^ (n)) / sqrt(5))

unfib :: Integer -> Integer
unfib n = round ((2.078087 * (log $ fromIntegral n)) + 1.672276)

nextFib :: Integer -> Integer
nextFib x = fib ((unfib x) + 1)
