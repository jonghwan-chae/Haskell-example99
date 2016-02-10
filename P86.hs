--
-- Problem 86:: Node degree and graph coloration (Use Welch-Powell's algorithm to paint the nodes of a graph)

import Data.List 

data Graph a = Graph [a] [(a, a)]   deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]   deriving (Show, Eq)
data Friendly a = Edge [(a, a)]     deriving (Show, Eq)

--
-- Test Case 

graphG = Graph ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]

{-
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.  
-}

kcolor  :: Eq a => Graph a -> [(a, Int)]
kcolor g = []


{-
Example in Haskell: 

kcolor ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
[('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]
-}

{- 
The Welsh-Powell Algorithm

This class is intended to implement the Welsh-Powell algorithm for the problem of graph coloring. It provides a greedy algorithm that runs on a static graph.

This is an iterative greedy algorithm:
• Step 1: All vertices are sorted according to the decreasing value of their degree in a list V.
• Step 2: Colors are ordered in a list C.
• Step 3: The first non colored vertex v in V is colored with the first available color in C. available means a color that was not previously used by the algorithm.
• Step 4: The remaining part of the ordered list V is traversed and the same color is allocated to every vertex for which no adjacent vertex has the same color.
• Step 5: Steps 3 and 4 are applied iteratively until all the vertices have been colored.

-}

