module Main where

import Lib

main :: IO ()
main =
  someFunc

countToGray :: Binary -> Int -> IO ()
countToGray _ (-1) = return ()
countToGray bits n = do
  putStrLn $ bitString $ bits
  countToGray (incGray bits) (n-1)

countTo :: Binary -> Int -> IO ()
countTo _ (-1) = return ()
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

type Binary = [Bit]
type Bit = Bool

inc :: Binary -> Binary
inc [True] = [True, False]
inc [False] = [True]
inc bits | last bits == False = init bits ++ [True]
inc bits = (inc ( init bits)) ++ [False]

bitString :: Binary -> String
bitString [] = ""
bitString bits = (bitToString (head bits)) ++ (bitString (tail bits))

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

binaryToGray :: Binary -> Binary
binaryToGray xs = (head xs) : (binaryToGray' (xs))

binaryToGray' :: Binary -> Binary
binaryToGray' [_] = []
binaryToGray' (x:x':xs) = (xor x x') : binaryToGray' (x':xs)

binaryToGrayAlt :: Binary -> Binary
binaryToGrayAlt binary = xor' binary (rShift binary)

rShift :: Binary -> Binary
rShift [] = []
rShift xs = False : init xs

lShift :: Binary -> Binary
lShift (x:xs) = xs ++ [False]

grayToBinary :: Binary -> Binary
grayToBinary bits = grayToBinary' False bits

grayToBinary' :: Bit -> Binary -> Binary
grayToBinary' bit [x] = [xor bit x]
grayToBinary' bit (x:xs) = new : (grayToBinary' new xs)
  where new = (xor bit x)

xor :: Bool -> Bool -> Bool
xor a b = a /= b

xor' :: Binary -> Binary ->Binary
xor' [] _ = []
xor' _ [] = []
xor' (x:xs) (y:ys) = (xor x y) : (xor' xs ys)

ruler :: Int -> Int
ruler n | (mod n 2) == 1 = 0
ruler n = 1 + ruler (quot n 2)

--iterative solution to towers of hanoi

--  1.  calculate the total number of moves required
--      2^n, where n is number of disks

--  2.  is n is even, swap dest and aux pegs

--  3.  for i = (1..n)
--        if i%3 == 1
--          move top disk from src pole to dest pole
--        if i%3 == 2
--          move top disk from src pole to aux pole
--        if i%3 == 0
--          mve top disk between aux and dest
