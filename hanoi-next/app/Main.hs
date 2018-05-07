module Main where

import Lib

main :: IO ()
main = do
  let currentState = "bba"
  let goalPeg = 'c'
  let goalDirection = "clockwise"
  printPeg currentState goalPeg goalDirection
  printDirection currentState goalPeg goalDirection
  --mapM (\x -> printPeg x goalPeg goalDirection) intermediateStates
  --mapM (\x -> printDirection x goalPeg goalDirection) intermediateStates
  return ()

-- "ccc" is left out as we won't move from there
intermediateStates = ["bba", "bbc", "abc", "acc"]

-- printPeg: currentState -> goalPeg -> goalDirection
-- prints next move using there pegs for reference
printPeg :: State -> Peg -> Direction -> IO ()
printPeg s p d = do
  -- get the binary of the current state
  let binary = stateToBinary s p
  -- get the binary of the next state
  let nextMove = increment binary
  -- find the pegs involved in the move
  let (from, to) = fromTo nextMove
  print ("move top disk from peg " ++ (show from) ++ " to peg " ++ (show to))

--printDirection: currentState -> goalPeg -> goalDirection
--prints moves using the disk and their direction, rather than pegs
printDirection :: State -> Peg -> Direction -> IO ()
printDirection s p d = do
  -- get binary of current state
  let current = stateToBinary s p
  -- get binary of next state
  let next = increment current
  -- find the disk to move using the ruler sequence
  let diskToMove = ruler $ binaryToInt next
  -- find the direction relative tot the goal
  let relativeDirection = direction (length s) diskToMove
  -- compensate for goal direction
  let directionToMove = if (relativeDirection == 1) then d else invert d
  print ("move disk " ++ (show diskToMove) ++ " " ++ directionToMove)

-- some types for clarification
type Bit = Bool
type Binary = [Bit]
type State = String
type Peg = Char
type Direction = String

--we can find the move number from any state of the puzzle present in the optimal solution
stateToBinary :: State -> Peg -> Binary
stateToBinary state goal = stateToBinary' (reverse state) goal True

stateToBinary' :: State -> Peg -> Bit -> Binary
stateToBinary' [] _ _ = []
stateToBinary' (x:xs) prePeg preBit = currentBit : (stateToBinary' (xs) x currentBit)
  where currentBit = if (x == prePeg) then preBit else not preBit

-- gets direction of disk relative to goal, odd+odd or even+even, same as goal
direction :: Int -> Int -> Int
direction n i
  | odd n = if odd i then 1 else -1
  | otherwise = if even i then 1 else -1

invert :: Direction -> Direction
invert "clockwise" = "anti-clockwise"
invert "anti-clockwise" = "clockwise"

--once we know the move number, we can find out which peg the next move will be from and to
fromTo :: Binary -> (Int, Int)
fromTo x = (from x, to x)

-- uses pegs pattern to find current from
from :: Binary -> Int
from x = (binaryToInt (bitwise (&&) x (decrement x))) `mod` 3

-- uses pegs pattern to find current to
to :: Binary -> Int
to x = (binaryToInt (increment (bitwise (||) x (decrement x)))) `mod` 3

-- converta a binary number to an int
binaryToInt :: Binary -> Int
binaryToInt bits = binaryToInt' 0 (reverse bits)

-- a helper for binaryToInt
binaryToInt' :: Int -> Binary -> Int
binaryToInt' _ [] = 0
binaryToInt' e (x:xs)
  | x = (2 ^ e) + (binaryToInt' (e+1) xs)
  | otherwise = binaryToInt' (e+1) xs

-- converts an int to binary representation with specified amount of bits
intToBinary :: Int -> Int -> Binary
intToBinary _ 0 = []
intToBinary x n
  | quot x (2^(n-1)) > 0 = True : intToBinary (mod x (2^(n-1))) (n-1)
  | otherwise = False : intToBinary x (n-1)

-- decrements a binary number
decrement :: Binary -> Binary
decrement [True] = [False]
decrement binary
  | (last binary) == True = (init binary) ++ [False]
  | otherwise = (decrement (init binary)) ++ [True]

-- increments a binary number
increment :: Binary -> Binary
increment [] = [True]
increment [False] = [True]
increment binary
  | (last binary) == False = (init binary) ++ [True]
  | otherwise = (increment (init binary)) ++ [False]

-- higher order function for bitwise operations across my binary (list of bools) datatypes
bitwise :: (Bool -> Bool -> Bool) -> Binary -> Binary -> Binary
bitwise _ [] [] = []
bitwise op (x:xs) (y:ys) = (op x y) : bitwise op xs ys

-- utilising binary to find ruler numbers
ruler :: Int -> Int
ruler x = rightmostOne $ intToBinary' x

-- where the rightmost has index 1
rightmostOne :: Binary -> Int
rightmostOne xs
  | (not $ last xs) = 1 + (rightmostOne $ init xs)
  | otherwise = 1

-- helper function
intToBinary' :: Int -> Binary
intToBinary' 0 = []
intToBinary' x = (intToBinary' (div x 2)) ++ [mod x 2 == 1]
