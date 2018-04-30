module Main where

import Lib

data Vertex = Vertex {
                          vertexLabel :: [Char]
                        , vertexNeighbors :: [[Char]]
                      } deriving (Show)

-- We define a graph as a list of vertexes.
-- Each vertex is a label, and neighbours.
data Graph = Graph [Vertex] deriving (Show)

-- Takes in a vertex, a list of vertexes, and returns true or false.
vertexInVertexes :: Vertex -> [Vertex] -> Bool
vertexInVertexes _ [] = False
-- If at least one vertex label in the list matches the input vertex label, the result will be true.
vertexInVertexes Vertex {vertexLabel = label} (x:y) = foldl (\ acc x -> vertexLabel x == label || acc) False (x:y)

graphVertexes :: Graph -> [[Char]]-> [Vertex]
graphVertexes (Graph []) _ = []
graphVertexes (Graph (x:y)) [] = x : y
graphVertexes (Graph (x:y)) keys = filter (\ z -> vertexLabel z `elem` keys) (x:y)

main :: IO ()
main = do
  let startState = "bba"
  let goalState = "bbb"
  let graph = threeDisk
  let state = (startState, (graph, goalState, []))
  printPath $ shortestPath $ findPaths state
  return ()

-- twoDisk = Graph [
--       Vertex "aa" ["ba", "ca"        ] 0
--     , Vertex "ab" ["bb", "ac", "cb"  ] 0
--     , Vertex "ac" ["bc", "ab", "cc"  ] 0
--     , Vertex "ba" ["aa", "ca", "bc"  ] 0
--     , Vertex "bb" ["ab", "cb"        ] 0
--     , Vertex "bc" ["ac", "cc", "ba"  ] 0
--     , Vertex "ca" ["cb", "aa", "ba"  ] 0
--     , Vertex "cb" ["ab", "bb", "ca"  ] 0
--     , Vertex "cc" ["ac", "bc"        ] 0
--   ]

threeDisk = Graph [
      Vertex "aaa" ["baa", "caa"]
    , Vertex "caa" ["aaa", "baa", "cba"]
    , Vertex "baa" ["aaa", "caa", "bca"]
    , Vertex "bca" ["baa", "cca", "aca"]
    , Vertex "cba" ["caa", "aba", "bba"]
    , Vertex "cca" ["bca", "aca", "ccb"]
    , Vertex "aca" ["bca", "cca", "aba"]
    , Vertex "aba" ["aca", "cba", "bba"]
    , Vertex "bba" ["cba", "aba", "bbc"]
    --
    , Vertex "ccb" ["cca", "acb", "bcb"]
    , Vertex "bbc" ["bba", "cbc", "abc"]
    , Vertex "acb" ["ccb", "bcb", "abb"]
    , Vertex "bcb" ["ccb", "acb", "bab"]
    , Vertex "cbc" ["bbc", "cac", "abc"]
    , Vertex "abc" ["bbc", "cbc", "acc"]
    , Vertex "abb" ["acb", "bbb", "cbb"]
    , Vertex "bab" ["bcb", "cab", "aab"]
    , Vertex "cac" ["cbc", "aac", "bac"]
    --
    , Vertex "acc" ["abc", "bcc", "ccc"]
    , Vertex "bbb" ["abb", "cbb"]
    , Vertex "cbb" ["abb", "bbb", "cab"]
    , Vertex "cab" ["bab", "cbb", "aab"]
    , Vertex "aab" ["bab", "cab", "aac"]
    , Vertex "aac" ["aab", "cac", "bac"]
    , Vertex "bac" ["cac", "aac", "bcc"]
    , Vertex "bcc" ["acc", "bac", "ccc"]
    , Vertex "ccc" ["acc", "bcc"]
  ]

--            current graph   goal     path
type State = (String, (Graph, String, [Vertex]))

type Path = [Vertex]

printPaths :: [Path] -> IO ()
printPaths [] = return ()
printPaths (x:xs) = do
  putStrLn "PATHSTART"
  printPath x
  putStrLn "PATHEND"
  printPaths xs

printPath :: Path -> IO()
printPath [] = return ()
printPath [x] = putStrLn $ vertexLabel x
printPath (x:xs) = do
  putStr $ vertexLabel x
  putStr " -> "
  printPath xs

findPaths :: State -> [Path]
findPaths (current, (graph, goal, path))
  | current == goal = [path ++ [head (graphVertexes graph [current])]]
  | otherwise = foldl (++) [] (map findPaths newStates)
      where
        -- get the current vertex from its label
        [currentVertex] = graphVertexes graph [current]
        -- make a list of all current's neighbours
        neighbourVertexes = graphVertexes graph (vertexNeighbors currentVertex)
        -- remove already visited vertexes from the list to remove cycles in the graph
        unvisited = [x | x <- neighbourVertexes, not $ elem (vertexLabel x) (map vertexLabel path)]
        -- add the current vertex to the path so far
        newPath = path ++ [currentVertex]
        -- find a path to the goal from all current's neighbours
        newStates = zip (map vertexLabel unvisited) (repeat (graph, goal, newPath))

shortestPath :: [Path] -> Path
shortestPath [x] = x
shortestPath (x:x':xs)
  | length x < length x' = shortestPath (x:xs)
  | otherwise = shortestPath (x':xs)

remove' :: [Vertex] -> [Vertex] -> [Vertex]
remove' [] ys = ys
remove' _ [] = []
remove' (x:xs) (ys) = remove' xs (remove x ys)

remove :: Vertex -> [Vertex] -> [Vertex]
remove _ [] = []
remove v (x:xs)
  | vertexLabel v == vertexLabel x = rest
  | otherwise = v : rest
  where rest = remove v xs

-- An impure function that takes a graph and performs input/output (IO).
printGraph :: Graph -> IO ()
printGraph (Graph []) = putStrLn ""
printGraph (Graph (x:y)) = do
  -- Print the first vertex.
  print x
  -- Print the rest of the vertexes.
  printGraph (Graph y)
  return ()
