--
-- Problem 73:: Lisp-like tree representation. 

import Data.List 

data Tree a = Node a [Tree a]
        deriving (Eq, Show)
{-
A multiway tree is composed of a root element and a (possibly empty) set of successors 
which are multiway trees themselves. A multiway tree is never empty. 
The set of successor trees is sometimes called a forest. 
-}

display :: Tree Char -> String 
display tree = case tree of 
    (Node a []) -> [a]
    (Node a ts) -> "(" ++ [a] ++ (foldr (\a b -> " " ++ (display a) ++ b) [] ts) ++ ")"


{-
Example in Haskell: 

Tree> display lisp tree1
"a"
Tree> display lisp tree2
"(a b)"
Tree> display lisp tree3
"(a (b c))"
Tree> display lisp tree4
"(b d e)"
Tree> display lisp tree5
"(a (f g) c (b d e))"

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
