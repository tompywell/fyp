module Main where

import Lib

-- a vertex has a name and a list of neighbours
data Vertex = Vertex {
                          vertexLabel :: [Char]
                        , vertexNeighbors :: [[Char]]
                      } deriving (Show)

--  a graph is just a list of vertexes
data Graph = Graph [Vertex] deriving (Show)

-- gets a list of vertexes from a graph using there labels
graphVertexes :: Graph -> [[Char]]-> [Vertex]
graphVertexes (Graph []) _ = []
graphVertexes (Graph (x:xs)) [] = x : xs
graphVertexes (Graph (x:xs)) labels = filter (\ z -> elem (vertexLabel z) labels) (x:xs)

main :: IO ()
main = do
  let startState = "bba"
  let goalState = "bbb"
  let graph = threeDisk
  let state = (startState, (graph, goalState, []))
  printPath $ shortestPath $ findPaths state

-- twoDisk = Graph [
--       Vertex "aa" ["ba", "ca"        ]
--     , Vertex "ab" ["bb", "ac", "cb"  ]
--     , Vertex "ac" ["bc", "ab", "cc"  ]
--     , Vertex "ba" ["aa", "ca", "bc"  ]
--     , Vertex "bb" ["ab", "cb"        ]
--     , Vertex "bc" ["ac", "cc", "ba"  ]
--     , Vertex "ca" ["cb", "aa", "ba"  ]
--     , Vertex "cb" ["ab", "bb", "ca"  ]
--     , Vertex "cc" ["ac", "bc"        ]
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

--            current, graph  goal    path
type State = (String, (Graph, String, [Vertex]))

type Path = [Vertex]

-- prints multiple paths
printPaths :: [Path] -> IO ()
printPaths [] = return ()
printPaths (x:xs) = do
  putStrLn "PATHSTART"
  printPath x
  putStrLn "PATHEND"
  printPaths xs

-- prints a single path
printPath :: Path -> IO()
printPath [] = return ()
printPath [x] = putStrLn $ vertexLabel x
printPath (x:xs) = do
  putStr $ vertexLabel x ++ " -> "
  printPath xs

-- an algorithm to find all possible paths from one node to another, without cycles (otherwise infinte)
findPaths :: State -> [Path]
findPaths (current, (graph, goal, path))
  -- if we are at the goal node, return the path to get here
  | current == goal = [path ++ [head (graphVertexes graph [current])]]
  -- if not at goal, visit all neighbours and search from there
  | otherwise = foldl (++) [] (map findPaths newStates)
      where
        -- get the current vertex from its label
        [currentVertex] = graphVertexes graph [current]
        -- make a list of all current's neighbours
        neighbourVertexes = graphVertexes graph (vertexNeighbors currentVertex)
        -- remove already visited vertexes from the list of neighbours to remove cycles in the path
        unvisited = [x | x <- neighbourVertexes, not $ elem (vertexLabel x) (map vertexLabel path)]
        -- add the current vertex to the path
        newPath = path ++ [currentVertex]
        -- find a path to the goal from all current's neighbours
        newStates = zip (map vertexLabel unvisited) (repeat (graph, goal, newPath))

-- given a list of all paths, this finds a shortest
shortestPath :: [Path] -> Path
shortestPath [x] = x
shortestPath (x:x':xs)
  | length x < length x' = shortestPath (x:xs)
  | otherwise = shortestPath (x':xs)

-- An impure function that takes a graph and performs input/output (IO).
printGraph :: Graph -> IO ()
printGraph (Graph []) = return ()
printGraph (Graph (x:y)) = do
  -- Print the first vertex.
  print x
  -- Print the rest of the vertexes.
  printGraph (Graph y)
  return ()
