--
-- Problem 89:: Bipartite graphs. Write a predicate that finds out whether a given graph is bipartite. 

import Data.List 
import Data.Maybe

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

--
-- Test Case
graphG1 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]
graphG2 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)]

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

bipartite :: Ord a => Graph a -> Bool
bipartite (Graph vertexs edges) = biPart (tail vertexs) edges [head vertexs] []
    where 
        biPart rest es set1 set2
            | rest == [] = (checkSet es set1) && (checkSet es set2)
            | otherwise  = let newset = makeSet es set1 
                           in biPart (remove rest newset) es newset set1
        remove from set = filter (\x -> not $ elem x set) from 
        makeSet es set1 = insSort $ do
            v <- set1
            (getConnected v es)
        checkSet es cv = and $ do
            chkV <- cv
            let conV = getConnected chkV es
            [and $ map (\x -> not $ elem x cv) conV]

getConnected :: Eq a => a -> [(a, a)] -> [a]
getConnected a edges = do
    e <- edges
    let check = checkEdge a e in if (check /= Nothing) then [fromJust check] else []
    where checkEdge a (f, s) 
            | f == a     = Just s
            | s == a     = Just f
            | otherwise  = Nothing

--
-- Module : Insertion Sort / removing redundancy

insSort :: Ord a => [a] -> [a]
insSort []     = []
insSort (a:as) = ins a (insSort as)

ins :: Ord a => a -> [a] -> [a]
ins e []     = [e]
ins e (a:as) = if (e == a) then a : as
               else if (e < a) then e : a : as
                               else a : (ins e as)
{-
Example in Haskell: 

bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
True
bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
False
-}

