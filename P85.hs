--
-- Problem 85:: Graph isomorphism 

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

graphG1 = [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
iso graphG1 graphH1
True
-}

