--
-- Problem 82:: Cycle from a given node 
-- Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. 
-- The predicate should return all cycles via backtracking. 

import Data.List 

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

cycles :: Eq a => a -> [(a, a)] -> [[a]]
cycles p1 e = do 
    item <- getPath p1 e []
    if (0 /= length item) && ((head item) == (last item)) then [item] else []

getPath :: Eq a => a -> [(a, a)] -> [a] -> [[a]]
getPath p1 edges checkrt 
    | (elem p1 checkrt) = [checkrt ++ [p1]]
    | otherwise     = do
        p  <- getReachableNode p1 edges
        getPath p edges (checkrt ++ [p1])

getReachableNode :: Eq a => a -> [(a, a)] -> [a]
getReachableNode p1 es = do
    edge <- es
    if (p1 == fst edge) then [snd edge] else []


{-
Example in Haskell: 

graph> cycles 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[2,3,4,2]]
graph> cycles 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
-}

