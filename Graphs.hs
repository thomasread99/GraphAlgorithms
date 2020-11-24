{-
  G54PAD Project in Advanced Algorithms and Data Structures
    Autumn 2020

  Assignment 2 
    Graph Algorithms

  Student Name: Thomas Read
  Student ID: 4296726

  Complete this Haskell file by providing definitions
  of the following functions:

  adjList
  adjMatrix

  adjListW
  adjMatrixW

  dijkstra
  floydWarshall

  You are allowed to define any other auxiliary function you need.

-}


module Graphs where

import Numeric

-- We assume that the vertices are numbered 0 ... n-1

{- In and adjacency list al, the i-th element al!!i
   is a list of all the vertices j such that
   there is an edge from i to j
-}

type AdjList = [[Int]]

{- In and adjacency matrix am, the element with
   coordinates i,j, that is am!!i!!j
   is True if there is an edge from i to j
      False if there is no edge from i to j
-}

type AdjMatrix = [[Bool]]

-- Suppose we're given a graph as a list of edges (i,j)
-- Generate the Adjacency List and Adjacency Matrix representations

-- GENERATION OF ADJACENCY LIST
adjList :: [(Int,Int)] -> AdjList
adjList n = [filter (>=0) ([if (elem (y,x) n) then x else -1 | x <- [0..(graphMax n)]]) | y <- [0..(graphMax n)]]


-- GENERATION OF ADJACENCY MATRIX
adjMatrix :: [(Int,Int)] -> AdjMatrix
adjMatrix [] = []
adjMatrix n = [[elem (x,y) n | x <- [0..(graphMax n)]] | y <- [0..(graphMax n)]]                          
              
--Auxiliary function to find the maximum node value in the graph
graphMax :: [(Int,Int)] -> Int
graphMax x = maximum (concat [[a,b] | (a,b) <- x])

--------------------------------------------------------

-- WEIGHTED GRAPHS: every edge has a "weight"

{- In an adjacency list al, the i-th element al!!i
   contains all the pairs (j,w) such that
   there is an edge from i to j with weight w
-}

type WAdjList = [[(Int,Float)]]

{- In an adjacency matrix am, the element with
   coordinates i,j is
     Nothing if there is no edge from i to j
     (Just w) if there is an edge from i to j with weight w
-}

type WAdjMatrix = [[Maybe Float]]

{- We can also represent a weighted graphs by a list of edges
   (i,j,w) denotes an edge from i to j with weight w
-}

type Edges = [(Int,Int,Float)]

-- GENERATION OF ADJACENCY LIST
--   from a list of edges

adjListW :: Edges -> WAdjList
adjListW n = [filter (>=(0,0.0)) ([if (elem(y,x,z) n) then (x,z) else (-1,-1.0) | x <- [0..(wGraphMax n)], z <- [0.0..(weightMax n)]]) | y <- [0..(wGraphMax n)]]

--Auxiliary function to find the maximum node value in the graph
wGraphMax :: Edges -> Int
wGraphMax x = maximum (concat [[a,b] | (a,b,c) <- x])

--Auxiliary function to find the maximum weight in the graph
weightMax :: Edges -> Float
weightMax x = maximum (concat [[c] | (a,b,c) <- x])


-- GENERATION OF ADJACENCY MATRIX
--   from a list of edges

adjMatrixW :: Edges -> WAdjMatrix
adjMatrixW [] = []
adjMatrixW n = [[if (elem(y,x,z) n) then Just z else Nothing | x <- [0..(wGraphMax n)], let z = (getW y x n)] | y <- [0..(wGraphMax n)]] 

--Auxiliary function to find the weight to a specific node
getW :: Int -> Int -> Edges -> Float
getW x y [] = -1.0
getW x y ((a,b,c) : ns) = if (x == a) && (y == b) then c
                                                  else getW x y ns

-- DIJKSTRA'S ALGORITHM

{- 
   Given an adjacencly list al and a source vertex s
   return the list of minimum distances of vertices from s:
   (dijkstra al s)!!j is the minimum distance from s to j
-}

type PriorityQueue = [(Int,Float)]

dijkstra :: WAdjList -> Int -> [Maybe Float]
dijkstra g s = orderResult(dijkstra' g s (initPQ g s))

--Recursive Dijkstra's function
dijkstra' :: WAdjList -> Int -> PriorityQueue -> [(Int, Maybe Float)]
dijkstra' g s [] = []
dijkstra' g s ((k,v) : xs) = let answer = v
                                 key = k
                                 newPQ = (reorderPQ (relaxPQ xs s v g))                             
                             in if answer == 99.0 then [(key,Nothing)] ++ (dijkstra' g (fst (head newPQ)) newPQ)                                
                                else [(key,Just answer)] ++ (dijkstra' g (fst (head newPQ)) newPQ)                            

--Auxiliary function order the final result correctly
orderResult :: [(Int, Maybe Float)] -> [Maybe Float]
orderResult [] = []
orderResult ((k,v) : xs) = orderResult smaller ++ [v] ++ orderResult larger
                           where
                              smaller = [a | a <- xs, a <= (k,v)]
                              larger = [b | b <- xs, b > (k,v)]

--Auxiliary function to relax the values in the priority queue
relaxPQ :: PriorityQueue -> Int -> Float -> WAdjList -> PriorityQueue
relaxPQ [] s sw g = []
relaxPQ ((k,v) : xs) s sw g = if isEdge s k g then (if (getWeight s k g + sw) < v then [(k,(getWeight s k g + sw))] ++ relaxPQ xs s sw g
                                                                                  else [(k,v)] ++ relaxPQ xs s sw g) 
                                              else [(k,v)] ++ relaxPQ xs s sw g                 
                      
--Auxiliary function to initialise the priority queue
initPQ :: WAdjList -> Int -> PriorityQueue
initPQ g s = let first = [(s,0.0)]
                 rest = [(k,99.0) | k <- [0..((length g)-1)], k /= s]
             in first ++ rest 

--Auxiliary function to reorder the priority queue using quicksort
reorderPQ :: PriorityQueue -> PriorityQueue
reorderPQ [] = []
reorderPQ ((a,b) : xs) = reorderPQ smaller ++ [(a,b)] ++ reorderPQ larger
                         where
                            larger = [(c,d) | (c,d) <- xs, d > b]
                            smaller = [(e,f) | (e,f) <- xs, f <= b]

--Auxiliary function to determine is there is an edge between two nodes
isEdge :: Int -> Int -> WAdjList -> Bool
isEdge i j g = isEdge' j (g!!i)

--Recursive function to determine if there is an edge between two nodes
isEdge' :: Int -> [(Int,Float)] -> Bool
isEdge' x [] = False
isEdge' x ((a,b) : ns) = if (x == a) then True
                                     else isEdge' x ns

--Auxiliary function to get the weight between two nodes
getWeight :: Int -> Int -> WAdjList -> Float
getWeight s d g = getWeight' d (g!!s)

--Recursive function to get weight
getWeight' :: Int -> [(Int,Float)] -> Float
getWeight' d ((a,b) : ns) = if (d == a) then b
                                        else getWeight' d ns

-- FLOYD-WARSHALL ALGORITHM

{-
   Given an adjacency matrix am, return the matrix of minimum distances:
   (floydWarshall am)!!i!!j is
     Nothing if there is no path from i to j
     (Just x) if the shortest path from i to j has length x
-}

floydWarshall :: WAdjMatrix -> WAdjMatrix
floydWarshall g = floydWarshall' 0 (initAnsMatrix g)   

--Recursive Floyd-Warshall function
floydWarshall' :: Int -> WAdjMatrix -> WAdjMatrix
floydWarshall' k g = if k == (length g) then g 
                                        else floydWarshall' (k+1) ([[if sum == Nothing then v else (if (compareValues sum v) then sum else v) | i <- [0..((length g)-1)], let v = g!!i!!j, let sum = (addFloats (g!!i!!k) (g!!k!!j))] | j <- [0..((length g)-1)]])

--Auxiliary function to initialise the answer matrix
initAnsMatrix :: WAdjMatrix -> WAdjMatrix
initAnsMatrix g = [[if i==j then Just 0.0 else v | i <- [0..((length g)-1)], v <- [g!!i!!j]] | j <- [0..((length g)-1)]]

--Auxiliary function to add two floats together
addFloats :: Maybe Float -> Maybe Float -> Maybe Float
addFloats Nothing Nothing = Nothing
addFloats (Just w) Nothing = Nothing
addFloats Nothing (Just w2) = Nothing
addFloats (Just w) (Just w2) = Just (w + w2)

--Auxiliary function to compare two float values together (needed due to using Nothing in place of infinite)
compareValues :: Maybe Float -> Maybe Float -> Bool
compareValues (Just w) Nothing = True
compareValues (Just w) (Just w2) = if w < w2 then True
                                             else False

{-TO DO   
   -Problem with floyd when two node's don't have a direct connection (init to 0 or something)
   -Error handling if node not exist dijkstras   
-}