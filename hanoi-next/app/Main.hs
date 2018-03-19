module Main where

import Lib

main :: IO ()
main = someFunc

type Bit = Bool
type Binary = [Bit]
type State = String
type Peg = Char

--we can find the move number from any state of the puzzle present in the optimal solution

stateToBinary :: State -> Peg -> Binary
stateToBinary state goal = stateToBinary' state goal True

stateToBinary' :: State -> Peg -> Bit -> Binary
stateToBinary' [] _ _ = []
stateToBinary' (x:xs) previousPeg previousBit = currentBit : (stateToBinary' (xs) currentPeg currentBit)
  where
    currentPeg = x
    currentBit = if (currentPeg == previousPeg) then previousBit else not previousBit

--once we know the move number, we can find out which peg the next move will be from and to

-
magic :: Binary -> (Int, Int)
magic binary = (from, to)
  where
    -- from peg (m & m - 1) % 3 to peg ((m | m - 1) + 1) % 3
    from = (binaryToInt (bitwise (&&) binary (decrement binary))) `mod` 3
    to = (binaryToInt (increment ((bitwise (||) binary (decrement binary))))) `mod` 3

binaryToInt :: Binary -> Int
binaryToInt bits = binaryToInt' 0 (reverse bits)

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
