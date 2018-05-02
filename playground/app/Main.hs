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

type Binary = [Bit]
type Gray = [Bit]
type Bit = Bool

type Digit = Int
type Decimal = [Digit]

incDecimal :: Decimal -> Decimal
incDecimal [] = [1]
incDecimal digits
  | last digits == 9 = (incDecimal $ init digits) ++ [0]
  | otherwise = init digits ++ [(last digits) + 1]

generateDecimal :: Int -> [Decimal]
generateDecimal 0 = [[]]
--generateDecimal 1 = map (\x -> [x]) [0..9]
generateDecimal n = foldl (++) [] (map (\x -> prepend x previous) [0..9])
  where previous = generateDecimal (n-1)

prepend :: Int -> [Decimal] -> [Decimal]
prepend newHead lists = map (newHead : ) lists

incBinary :: Binary -> Binary
incBinary [] = [True]
incBinary xs | last xs == False = init xs ++ [True]
incBinary xs = (incBinary $ init xs) ++ [False]

inc :: Binary -> Binary
inc [] = [True]
inc bits
  | last bits == False = init bits ++ [True]
  | otherwise = (inc $ init bits) ++ [False]

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

decGray :: Gray -> Gray
decGray oldGray = prependLeadingZeros ((length oldGray) - (length newGray)) newGray
  where
    newGray = (binaryToGray (intToBinary (pred (binaryToInt (grayToBinary oldGray)))))

prependLeadingZeros :: Int -> Gray -> Gray
prependLeadingZeros 0 gray = gray
prependLeadingZeros n gray = False : (prependLeadingZeros (n-1) gray)

binaryToInt :: Binary -> Int
binaryToInt bits = binaryToInt' bits 0

binaryToInt' :: Binary -> Int -> Int
binaryToInt' [] _ = 0
binaryToInt' bits i
 | last bits = (2 ^ i) + binaryToInt' (init bits) (i + 1)
 | otherwise = binaryToInt' (init bits) (i + 1)

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

binaryToGray :: Binary -> Gray
binaryToGray bits = map (\(x, y) -> x /= y) (zip bits (rShift bits))

grayToBinary :: Gray -> Binary
grayToBinary bits = grayToBinary' False bits

grayToBinary' :: Bit -> Gray -> Binary
grayToBinary' carry [x] = [carry /= x]
grayToBinary' carry (x:xs) = new : (grayToBinary' new xs)
  where new = carry /= x

rShift :: Binary -> Binary
rShift [] = []
rShift xs = False : init xs

lShift :: Binary -> Binary
lShift (x:xs) = xs ++ [False]

ruler :: Int -> Int
ruler n | (mod n 2) == 1 = 1
ruler n = 1 + ruler (quot n 2)

intToBinary :: Int -> Binary
intToBinary 0 = []
intToBinary x = (intToBinary (div x 2)) ++ [mod x 2 == 1]

ruler' :: Int -> Int
ruler' x = rightmostOne $ intToBinary x

rightmostOne :: Binary -> Int
rightmostOne xs
  | last xs = 1
  | otherwise = (rightmostOne $ init xs) + 1

leftmostOne :: Binary -> Int
leftmostOne [True] = 1
leftmostOne (False:xs) = leftmostOne xs
leftmostOne (True:xs) = 1 + length xs

primes :: [Int]
primes = sieve [2..]

sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

count :: Int -> [Binary]
count 0 = []
count 1 = [[False],[True]]
count n = (addHead False (count (n-1))) ++ (addHead True (count (n-1)))

addHead :: Bit -> [Binary] -> [Binary]
addHead b [] = []
addHead b (x:xs) = (b:x) : addHead b xs

grayCodes :: Int -> [Binary]
grayCodes 0 = [[]]
grayCodes n = firstHalf ++ secondHalf
  where
    previous = grayCodes (n-1)
    firstHalf = map (\x -> False : x) previous
    secondHalf = map (\x -> True : x) (reverse previous)

generateBinary :: Int -> [Binary]
generateBinary 0 = [[]]
generateBinary n = map (\x -> False : x) (generateBinary $ n-1) ++
                  map (\x -> True : x) (generateBinary $ n-1)

grayAddBit :: [Binary] -> [Binary]
grayAddBit previous = firstHalf ++ secondHalf
  where
    firstHalf = map (\x -> False : x) previous
    secondHalf = map (\x -> True : x) (reverse previous)
