--
-- Problem 81:: Path from one node to another one
-- Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b. 

import Data.List 
import Data.Maybe

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths p1 p2 e = do 
    item <- getPath p1 e []
    let index = elemIndex p2 item
    if index /= Nothing then [take (1 + fromJust index) item] else []   
    
getPath :: Eq a => a -> [(a, a)] -> [a] -> [[a]]
getPath p1 edges checkrt 
    | (elem p1 checkrt) = [checkrt]
    | otherwise     = do
        p  <- getReachableNode p1 edges
        getPath p edges (checkrt ++ [p1])

getReachableNode :: Eq a => a -> [(a, a)] -> [a]
getReachableNode p1 es = do
    edge <- es
    if (p1 == fst edge) then [snd edge] else []



{-
Example in Haskell: 

paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[1,2,3,4],[1,3,4]]
paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
-}

