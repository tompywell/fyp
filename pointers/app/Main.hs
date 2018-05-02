module Main where

import Lib
import Data.IORef
import System.IO.Unsafe
import Control.Monad

main :: IO ()
main = do
  myTree <- newIORef Empty --create the pointer tree with no nodes
  mapM (insert myTree) [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15] --insert all these values into the binary tree
  print myTree  --a very basic 'pretty print' of the tree
  morris myTree --an in-order traversal of the tree
  prettyPrint myTree

data Tree = Empty
          | Node (IORef Tree) Int (IORef Tree)

instance Show Tree where
  show Empty = "."
  show (Node left val right) = "(" ++ (show left) ++ (show val) ++ (show right) ++ ")"

instance (Show a) => Show (IORef a) where
  show a = show (unsafePerformIO (readIORef a))

prettyPrint :: IORef Tree -> IO ()
prettyPrint tree = prettyPrint' tree 0

prettyPrint' :: IORef Tree -> Int -> IO ()
prettyPrint' tree n = do
  node <- readIORef tree
  if (isEmpty node)
    then return ()
    else do
      let (l, v, r) = (getLeft node, getVal node, getRight node)
      leftTree <- readIORef l
      rightTree <- readIORef r
      indent n
      putStrLn $ show v
      when (not (isEmpty leftTree)) (prettyPrint' l (n+1))
      when (not (isEmpty rightTree)) (prettyPrint' r (n+1))

indent :: Int -> IO ()
indent 0 = return ()
indent n = do
  putStr "| "
  indent (n-1)

insert :: IORef Tree -> Int -> IO ()
insert r v = do
  root <- readIORef r
  if (isEmpty root)
    then do
      left <- newIORef Empty
      right <- newIORef Empty
      writeIORef r (Node left v right)
    else do
      if (v < (getVal root))
        then insert (getLeft root) v
        else insert (getRight root) v

morris :: IORef Tree -> IO ()
morris t = do
  current <- readIORef t
  if (not $ isEmpty current)
    then do
      let (l, v, r, p) = (getLeft current, getVal current, getRight current, predecessor current)
      left <- readIORef l
      if (isEmpty left)
        then do
          putStr $ (show v) ++ " "
          morris r
        else do
          preRef <- p
          preTree <- readIORef preRef
          let rightRef = getRight preTree
          rightTree <- readIORef rightRef
          if (isEmpty rightTree)
            then do
              writeIORef rightRef current
              morris l
            else do
              setRightToNull preRef
              putStr $ (show v) ++ " "
              morris r
    else do
      putStrLn ""
      return ()

setRightToNull :: IORef Tree -> IO ()
setRightToNull node = do
  n <- readIORef node
  empty <- newIORef Empty
  writeIORef node (Node (getLeft n) (getVal n) empty)

predecessor :: Tree -> IO (IORef Tree)
predecessor (Node l v _ ) = do
  rightmost v l

rightmost :: Int -> IORef Tree -> IO (IORef Tree)
rightmost v t = do
  currentTree <- readIORef t
  let rightRef = getRight currentTree
  rightTree <- readIORef rightRef
  if (isEmpty rightTree || (getVal rightTree) == v)
    then return t
    else rightmost v rightRef

getVal :: Tree -> Int
getVal (Node _ x _) = x

getLeft :: Tree -> IORef Tree
getLeft (Node l _ _) = l

getRight :: Tree -> IORef Tree
getRight (Node _ _ r) = r

isEmpty :: Tree -> Bool
isEmpty Empty = True
isEmpty _ = False
