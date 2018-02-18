> module Main where

> import Lib
> import Data.Bits

> main :: IO ()
> main = prettyPrint $ map moveToCyclic (hanoi 20)

> type Direction = String

> hanoi :: Int -> [(Int, Int)]
> hanoi n = map (\x -> ((x .&. (x-1)) `mod` 3, ((x .|. (x-1)) + 1) `mod` 3)) [1..shift 1 n]

> moveToCyclic :: (Int, Int) -> (Int, Direction)
> moveToCyclic (0, 1) = (0, "Clockwise")
> moveToCyclic (1, 2) = (1, "Clockwise")
> moveToCyclic (2, 0) = (2, "Clockwise")
> moveToCyclic (x, y) = (x, "Anti-Clockwise")

> prettyPrint :: [(Int, Direction)] -> IO ()
> prettyPrint [] = return ()
> prettyPrint ((x, x'):xs) = do putStrLn ((towerPos x) ++ " " ++ (show x'))
>                               prettyPrint xs

> towerPos :: Int -> String
> towerPos 0 = "Top"
> towerPos 1 = "Right"
> towerPos 2 = "Left"

To move n disks clockwise
    where n is odd
      cycle the smallest disk clockwise
      after every move, do the one other legal move
    where n is even
     cycle the smallest disk anti-clockwise
     after every move, do the one other legal move

This works, for any number of discs, but why?
Simply stating it doesn't help in understanding how the solution is
constructed.

induction, assume we can move n discs from one peg to another
using this, prove we can move n+1 discs from one peg to another

this solution halts
moving n discs from A to B, we dont know how to move discs from this pole
moving the smallest disc from a to c, then move remaining discs from a to b
again no hypothesis for this

when moving an even number of discs in direction D,
all even numbered discs will cycle in direction D
all odd numbered discs will cycle in direction D'

when moving an odd number of discs in direction D,
all even numbered discs will cycle in direction D'
all odd numbered discs will cycle in direction D

summed up, the smallest disc (always n=1, odd) should cycle direction
opposite to D if N is even, and the same if N is odd

the movement of discs from turn to turn
always alterantes between the smnallest disc and another disc

It's easy to see this.
2 consecutive moves of the smallest disc will always be a waste,
as we could either complete the same state change in either one move,
or no moves

and 2 consecutive moves of any disc other than the smallest
will always have no effect on the state
since the peg with the smallest disc will not be involved

a sequence of numbers is 'alternating' if it satisfies the following 2 properties:
1. consecutive elements alternate between 1 and another number > 1
2. a non empty sequence begins and ends with 1

alt(xs) means xs has these properties
