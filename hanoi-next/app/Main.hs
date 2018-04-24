module Main where

import Lib

main :: IO ()
main = do
  let currentState = "bba"
  let goalPeg = 'c'
  let goalDirection = "clockwise"
  --printPeg currentState goalPeg goalDirection
  --printDirection currentState goalPeg goalDirection
  --mapM (\x -> printPeg x goalPeg goalDirection) optimalStates
  mapM (\x -> printDirection x goalPeg goalDirection) optimalStates
  return ()

optimalStates = ["bba", "bbc", "abc", "acc"]

--printPeg: currentState -> goalPeg -> goalDirection
printPeg :: State -> Peg -> Direction -> IO ()
printPeg s p d = do
  let binary = stateToBinary s p
  let nextMove = increment binary
  let (from, to) = magic nextMove
  let disk = ruler $ binaryToInt nextMove
  let diskDirection = direction (length s) disk
  let directionString = if (diskDirection == 1) then d else invert d
  print ("move disk " ++ (show disk) ++ " from peg " ++ (show from) ++ " to peg " ++ (show to))
  --print ("move disk " ++ (show disk) ++ " " ++ directionString)
  return ()

--printDirection: currentState -> goalPeg -> goalDirection
printDirection :: State -> Peg -> Direction -> IO ()
printDirection s p d = do
  let binary = stateToBinary s p
  let nextMove = increment binary
  let (from, to) = magic nextMove
  let disk = ruler $ binaryToInt nextMove
  let diskDirection = direction (length s) disk
  let directionString = if (diskDirection == 1) then d else invert d
  --print ("move disk " ++ (show disk) ++ " from peg " ++ (show from) ++ " to peg " ++ (show to))
  print ("move disk " ++ (show disk) ++ " " ++ directionString)
  return ()

type Bit = Bool
type Binary = [Bit]
type State = String
type Peg = Char
type Direction = String

--we can find the move number from any state of the puzzle present in the optimal solution

stateToBinary :: State -> Peg -> Binary
stateToBinary state goal = stateToBinary' (reverse state) goal True

direction :: Int -> Int -> Int
direction n i
  | odd n = if odd i then 1 else -1
  | otherwise = if even i then 1 else -1

invert :: Direction -> Direction
invert "clockwise" = "anti-clockwise"
invert "anti-clockwise" = "clockwise"

stateToBinary' :: State -> Peg -> Bit -> Binary
stateToBinary' [] _ _ = []
stateToBinary' (x:xs) previousPeg previousBit = currentBit : (stateToBinary' (xs) currentPeg currentBit)
  where
    currentPeg = x
    currentBit = if (currentPeg == previousPeg) then previousBit else not previousBit

--once we know the move number, we can find out which peg the next move will be from and to

magic :: Binary -> (Int, Int)
magic binary = (from, to)
  where
    -- from peg (m & m - 1) % 3 to peg ((m | m - 1) + 1) % 3
    from = (binaryToInt (bitwise (&&) binary (decrement binary))) `mod` 3
    to = (binaryToInt (increment (bitwise (||) binary (decrement binary)))) `mod` 3

binaryToInt :: Binary -> Int
binaryToInt bits = binaryToInt' 0 (reverse bits)

intToBinary :: Int -> Int -> Binary
--intToBinary _ 0 = []
intToBinary _ 0 = []
intToBinary x n
  | quot x (2^(n-1)) > 0 = True : intToBinary (mod x (2^(n-1))) (n-1)
  | otherwise = False : intToBinary x (n-1)

binaryToInt' :: Int -> Binary -> Int
binaryToInt' _ [] = 0
binaryToInt' e (x:xs)
  | x = (2 ^ e) + (binaryToInt' (e+1) xs)
  | otherwise = binaryToInt' (e+1) xs

decrement :: Binary -> Binary
decrement [True] = [False]
decrement binary
  | (last binary) == True = (init binary) ++ [False]
  | otherwise = (decrement (init binary)) ++ [True]

increment :: Binary -> Binary
increment [] = [True]
increment [False] = [True]
increment binary
  | (last binary) == False = (init binary) ++ [True]
  | otherwise = (increment (init binary)) ++ [False]

bitwise :: (Bool -> Bool -> Bool) -> Binary -> Binary -> Binary
bitwise _ [] [] = []
bitwise op (x:xs) (y:ys) = (op x y) : bitwise op xs ys

ruler :: Int -> Int
ruler x = rightmostOne $ intToBinary' x

rightmostOne :: Binary -> Int
rightmostOne xs
  | (not $ last xs) = 1 + (rightmostOne $ init xs)
  | otherwise = 1

intToBinary' :: Int -> Binary
intToBinary' 0 = []
intToBinary' x = (intToBinary' (div x 2)) ++ [mod x 2 == 1]
