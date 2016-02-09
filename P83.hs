--
-- Problem 83:: Construct all spanning trees 

import Data.List 

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

k4 = Graph ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}


spantree :: Eq a => Graph a -> [[(a, a)]]
spantree (Graph v e) = do 
    item <- filter (len) $ subsequences e
    if (isTree item) then [item] else []
    where 
        n = (length v) - 1
        len x = (n == length x) 


--
-- Module : check tree
isTree :: Eq a => [(a, a)] -> Bool
isTree (e:es) = let vs = [fst e, snd e] in addEdges vs es
    where addEdges vs edges = case edges of 
                []     -> True
                (e:es) -> if (elem (fst e) vs) then if (elem (snd e) vs) then False 
                                                                         else addEdges (vs ++ [snd e]) es 
                                               else if (elem (snd e) vs) then addEdges (vs ++ [fst e]) es 
                                                                         else addEdges vs (es ++ [e])

{-
Example in Haskell: 

length $ spantree k4
16
-}



