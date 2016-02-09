--
-- Problem 84:: Construct the minimal spanning tree 

import Data.List 

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

graphToAdj :: Eq a => Graph a -> Adjacency a
graphToAdj (Graph v e) = Adj (toAdjList v e)
    where 
        toAdjList vs es = do
            v <- vs
            [(v, toAdj v es)]
        toAdj v es = do  
            e <- es
            if (fst e == v) then [snd e] else if (snd e == v) then [fst e] else []


{-
Example in Haskell: 

prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
[(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
-}

