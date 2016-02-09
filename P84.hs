--
-- Problem 84:: Construct the minimal spanning tree 

import Data.List 

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

prim :: Eq a => [a] -> [(a, a, Int)] -> [(a, a, Int)]
prim vertexs edges = finds [head vertexs] edges ((length vertexs)-1)
    where 
        finds vertexs edges k 
            | k == 0    = []
            | otherwise = let edge = (minx $ checkverts vertexs edges) in edge : finds (addv edge vertexs) edges (k-1)
        addv (a, b, _) vertexs = if (elem a vertexs) then (vertexs ++ [b]) else (vertexs ++ [a])

        checkverts verts edges = do
            v <- verts
            check v verts edges
        check v1 vs edges = let es = filter (check'' v1 vs) edges in if (0 < length es) then [minx es] else []
            where check'' v vs (x, y, pri) = if ( (x == v && False == (elem) y vs) || (y == v && False == (elem) x vs) ) then True else False
        minx list = case list of (a:[]) -> a
                                 (a:as) -> compare'' a (minx as)
        compare'' e1@(_, _, p1) e2@(_, _, p2) = if (p1 < p2) then e1 else e2

{-
Example in Haskell: 

prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
[(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
-}

