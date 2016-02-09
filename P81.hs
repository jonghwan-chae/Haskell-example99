--
-- Problem 81:: Path from one node to another one
-- Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b. 

import Data.List 

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

paths :: Eq a => a -> a -> Friendly a
paths p1 p2 edges = [getPath p1 edges, getPath p2 edges]
    where getPath p edges = do
            getPath 


{-
Example in Haskell: 

paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[1,2,3,4],[1,3,4]]
paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
-}

