module Main where

import Lib
import Data.IORef
import System.IO.Unsafe

main :: IO ()
main = do
  myTree <- newIORef Empty --create the pointer tree with no nodes
  mapM (insert myTree) [4,2,6,1,3,5,7] --insert all these values into the binary tree
  print myTree  --a very basic 'pretty print' of the tree
  morris myTree --an in-order traversal of the tree

data Tree = Empty
          | Node (IORef Tree) Int (IORef Tree)

instance Show Tree where
  show Empty = "-"
  show (Node left val right) = "(" ++ (show left) ++ (show val) ++ (show right) ++ ")"

instance (Show a) => Show (IORef a) where
  show a = show (unsafePerformIO (readIORef a))

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
          print v
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
              print v
              morris r
    else
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
