module Main where

import Lib

main :: IO ()
main = do
  return ()

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest aux =
  (hanoi (n-1) src aux dest) ++
  [(src, dest)] ++
  (hanoi (n-1) aux dest src)

countMoves :: Integer -> IO ()
countMoves n =  putStrLn $ show $ length $ hanoi n "a" "b" "c"
