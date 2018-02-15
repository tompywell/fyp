module Main where

import Lib

main :: IO ()
main = do
  return ()

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start end _ = [(start, end)]
hanoi n start end aux = (hanoi (n-1) start end aux) ++
                        [(start, end)] ++
                        (hanoi (n-1) aux end start)

countMoves :: Integer -> IO ()
countMoves n =  putStrLn $ show $ length $ hanoi n "a" "b" "c"
