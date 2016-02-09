--
-- Problem 72:: Construct the bottom-up order sequence of the tree nodes. 

import Data.List 

data Tree a = Node a [Tree a]
        deriving (Eq, Show)
{-
A multiway tree is composed of a root element and a (possibly empty) set of successors 
which are multiway trees themselves. A multiway tree is never empty. 
The set of successor trees is sometimes called a forest. 
-}

bottom_up :: Eq a => Tree a -> [a] 
bottom_up tree = case tree of
    (Node a []) -> [a]
    (Node a ts) -> (foldr ((++).bottom_up) [] ts) ++ [a]


{-
Example in Haskell: 

Tree> bottom_up tree5
"gfcdeba"

-}


tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]


{-
main :: IO()
main = do
    putStrLn $ show $ countNodes tree65
    putStrLn $ show $ layout tree65
--    putStrLn $ show $ countNodes (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)))
-}    
