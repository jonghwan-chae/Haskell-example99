--
-- Problem 85:: Graph isomorphism 

import Data.List
import Data.Maybe

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

--
-- Test Case 

graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

--
-- Module : Check Graph isomorphism

iso :: Ord a => Graph a -> Graph a -> Bool
iso (Graph v1 e1) (Graph v2 e2) 
    | v1 /= v2       = False
    | otherwise      = and $ do
            v <- v1
            let adj1 = adjacency v e1
            let adj2 = adjacency v e2
            [(adj1 == adj2)]

--
-- Module : Calculate adjacency

adjacency :: Ord a => a -> [(a, a)] -> [a]
adjacency v [] = [v]
adjacency v e = let arr = v : adj_sum v e [] in insSort arr

adj_sum :: Eq a => a -> [(a, a)] -> [a] -> [a]
adj_sum v e ret
    | (elem v ret) = []
    | otherwise    = do
            vs <- getEdges v e
            [vs] ++ (adj_sum vs e (ret++[v]))


getEdges :: Eq a => a -> [(a, a)] -> [a]
getEdges a edges = do
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

graphG1 = [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
iso graphG1 graphH1
True
-}

