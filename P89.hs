--
-- Problem 89:: Bipartite graphs. Write a predicate that finds out whether a given graph is bipartite. 


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

bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
True
bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
False
-}

