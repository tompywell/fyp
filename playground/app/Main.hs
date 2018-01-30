module Main where

import Lib

main :: IO ()
main =
  someFunc

countToGray :: Binary -> Int -> IO ()
countToGray _ 0 = return ()
countToGray bits n = do
  putStrLn $ bitString $ bits
  countToGray (incGray bits) (n-1)

countTo :: Binary -> Int -> IO ()
countTo _ 0 = return ()
countTo bits n = do
  putStrLn $ bitString $ bits
  countTo (inc bits) (n-1)

primeFac :: Int -> [Int]
primeFac n = primeFac' 2 n

primeFac' :: Int -> Int -> [Int]
primeFac' i n
  | n==1 = []
  | (mod n i) == 0 = i : primeFac' i (quot n i)
  | otherwise = primeFac' (i+1) n

lcm' :: Int -> Int -> Int
lcm' a b = quot (a*b) (gcd' a b)

lcmList :: [Int] -> Int
lcmList xs = hof xs lcm'

gcd' :: Int -> Int -> Int
gcd' a b
  | a == b = a
  | a < b = gcd' a (b-a)
  | a > b = gcd' (a-b) b

gcdList :: [Int] -> Int
gcdList xs = hof xs gcd'

hof :: [Int] -> (Int -> Int -> Int) -> Int
hof [] f = 0
hof [x] f = x
hof (x:x':[]) f = f x x'
hof (x:x':xs) f = hof ((f x x'):xs) (f)

fib :: Int -> Int
fib n | n < 2 = 1
fib n = fib (n-1) + fib (n-2)

type Binary = [Bool]
type Bit = Bool

inc :: Binary -> Binary
inc [True] = [True, False]
inc [False] = [True]
inc bits | last bits == False = init bits ++ [True]
inc bits = (inc ( init bits)) ++ [False]

bitString :: Binary -> String
bitString [] = ""
bitString bits = (bitToString (head bits)) ++ (bitString (tail bits))
-- bitString bits | head bits == True = "1" ++ (bitString (tail bits))
-- bitString bits | head bits == False = "0" ++ (bitString (tail bits))

addParityBit :: Binary -> Binary
addParityBit bits | ((mod (length (filter (True ==) bits)) 2) == 0) = (bits ++ [True])
addParityBit bits = bits ++ [False]

bitToString :: Bit -> String
bitToString True = "1"
bitToString False = "0"

incGray :: Binary -> Binary
incGray bits = init $ incGrayHelper $ addParityBit bits

incGrayHelper :: Binary -> Binary
incGrayHelper (x:xs) | willToggleMSB xs = not x : incGrayHelper' xs
incGrayHelper (x:xs) = x : incGrayHelper' xs

incGrayHelper' :: Binary -> Binary
incGrayHelper' [_] = [True]
incGrayHelper' (x:xs) | willToggleHead xs = not x : incGrayHelper' xs
incGrayHelper' (x:xs) = x : incGrayHelper' xs

willToggleMSB :: Binary -> Bool
willToggleMSB (_:xs) = allFalse xs

willToggleHead :: Binary -> Bool
willToggleHead (True:xs) = (allFalse xs)
willToggleHead _ = False

allFalse :: Binary -> Bool
allFalse [] = True
allFalse [x] = not x
allFalse (True:_) = False
allFalse (False:xs) = allFalse xs
