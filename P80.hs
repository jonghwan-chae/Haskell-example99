--
-- Problem 80:: Conversions

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

graphToAdj (Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')])
Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
-}

