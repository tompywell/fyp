module Main where

import Lib

main :: IO ()
main = someFunc

--sample input
--dijkstra source dest [(n1,n2,e1),(n3,n4,e2)...] [(source,0)]
--dijkstra 1 4 [(1,2,5),(1,3,10),(2,4,100),(3,4,20)] [(1,0)]
--dijkstra 1 5 [(1,2,2), (2,3,1), (3,5,3), (4,5,4), (1,4,5), (1,3,8)] [(1,0)]

--ouput
-- a list of tuples with each tuple (n1,d1) representing the min. dist d1 of node n1 from source

inf = 100000

dijkstra  ::Int->Int->[(Int,Int,Int)]->[(Int,Int)]->[(Int,Int)]
dijkstra  s d adjList visited
		| (p,q) == (inf, inf) = visited
		| otherwise  = dijkstra p d adjList ((p,q):visited)
		where (p,q) = exploreNeighbours s visited adjList

exploreNeighbours ::Int->[(Int,Int)]->[(Int,Int,Int)]->(Int,Int)
exploreNeighbours source visited adjList =
  retMinNode (relax [(x,y,z) | (x,y,z) <- [(s,d,e) | (s,d,e)<-adjList, (belongsTo source visited)], not (belongsTo y visited)] visited)

belongsTo::Int->[(Int,Int)]->Bool
belongsTo _ [] = False
belongsTo s ((x,_):xs)
	| (x==s) = True
	| otherwise = belongsTo s xs

relax :: [(Int,Int,Int)]->[(Int,Int)]->[(Int,Int)]
relax [] visited = []
relax ((x,y,z):ns) visited = (y, (currDist x) + z):relax ns visited
	where currDist n = foldl(\acc t -> if (fst t == n) then (snd t) else acc) inf visited

retMinNode ::[(Int,Int)]->(Int,Int)
retMinNode [] = (inf,inf)
retMinNode arr = foldl (\acc t -> if ((snd t) < (snd acc)) then t else acc) (inf,inf) arr
