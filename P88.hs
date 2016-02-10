--
-- Problem 88:: Connected components (alternative solution) 

import Data.List 
import Data.Maybe

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

--
-- Test Case
graphG = Graph [1,2,3,4,5,6,7,8,9,10,11] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7),(6,10),(8,9),(9,11)]
{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

--
-- Module : connectedcomponents

connectedcomponents :: Eq a => Graph a -> [[a]]
connectedcomponents (Graph vertexs edges) = findConnected vertexs edges
    where 
        findConnected vs es 
            | vs == []    = []
            | otherwise   = let com = depthfirst (head vs) (Graph vs es) in [com] ++ findConnected (remove vs com) es
        remove vs com = filter (\x -> not $ elem x com) vs 

--
-- Module : DFS 

depthfirst :: Eq a => a -> Graph a -> [a]
depthfirst s (Graph v e) = searchdfs e [] s -- foldl (\b a -> (searchdfs e b a) ++ b) [] (getEdges s e) 
    where
        searchdfs e prev v  = 
            let (ret, next) = (makeRoute prev v, findEdge v e prev)
            in case next of Nothing  -> ret
                            (Just n) -> let ret = searchdfs e (makeRoute prev v) n in searchdfs e ret v
        makeRoute prev v = if (elem v prev) then prev else prev ++ [v]
        

findEdge :: Eq a => a -> [(a, a)] -> [a] -> Maybe a
findEdge v e prev = let vs = filter (\x -> False == elem x prev) $ getEdges v e 
                    in if (vs == []) then Nothing else Just (head vs)

getEdges :: Eq a => a -> [(a, a)] -> [a]
getEdges a edges = do
    e <- edges
    let check = checkEdge a e in if (check /= Nothing) then [fromJust check] else []
    where checkEdge a (f, s) 
            | f == a     = Just s
            | s == a     = Just f
            | otherwise  = Nothing


{-
Example in Haskell: 

connectedcomponents ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
[[1,2,3,4,5][6,7]]
-}

