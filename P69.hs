--
-- Problem 69::  a layout algorithm for drawing the tree in very compact layout while maintaining a certain symmetry in every node.

import Data.List 

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree2ds :: Tree Char -> String
tree2ds Empty          = "."
tree2ds (Branch a l r) = [a] ++ (tree2ds l) ++ (tree2ds r)

ds2tree :: String -> Maybe (Tree Char, Int)
ds2tree []  = Nothing
ds2tree ('.':as) = return (Empty, 1)
ds2tree str = do
    left <- ds2tree (drop 1 str) 
    right <- ds2tree (drop (1 + (snd left)) str)
    return (Branch (head str) (fst left) (fst right), 1 + (snd left) + (snd right))



{-
Example in Haskell: 

> layout tree65

-}

--
-- print Modules

tree65 = Branch 'n'
            (Branch 'k'
                (Branch 'c'
                    (Branch 'a' Empty Empty)
                    (Branch 'e'
                        (Branch 'd' Empty Empty)
                        (Branch 'g' Empty Empty)
                    )
                )
                (Branch 'm' Empty Empty)
            )
            (Branch 'u'
                (Branch 'p'
                    Empty
                    (Branch 'q' Empty Empty)
                )
                Empty
            )

{-
main :: IO()
main = do
    putStrLn $ show $ countNodes tree65
    putStrLn $ show $ layout tree65
--    putStrLn $ show $ countNodes (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)))
-}    
