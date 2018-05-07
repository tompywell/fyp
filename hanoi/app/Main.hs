module Main where

import Lib

main :: IO ()
main = do
  putStrLn $ show $ hanoi 5 "a" "b" "c"
  putStrLn $ show $ disks 5

type Peg = String
type Move = (Peg, Peg)

-- classic recursive solution to Tower of Hanoi
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest aux =
  (hanoi (n-1) src aux dest) ++
  [(src, dest)] ++
  (hanoi (n-1) aux dest src)

-- shows the disks involved in mves, rather than the pegs
disks :: Int -> [Int]
disks 0 = []
disks n =
  disks (n-1) ++
  [n] ++
  disks (n-1)
