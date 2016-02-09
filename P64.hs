--
-- Problem 64::  a layout algorithm for drawing the tree in a rectangular grid, to determine the position of each node
 
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

layout :: Eq a => Tree a -> Tree (a, (Int, Int))
layout tree  = makeLayout tree 1 1
    where makeLayout tree x y = case tree of 
                Empty            -> Empty
                (Branch val l r) -> Branch (val, (x + countNodes l, y)) (makeLayout l x (y+1)) (makeLayout r (x + 1 + countNodes l) (y+1))


countNodes :: Eq a => Tree a -> Int
countNodes Empty          = 0
countNodes (Branch _ l r) = 1 + (countNodes l) + (countNodes r)

--
-- print Modules

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

main :: IO()
main = do
    putStrLn $ show $ countNodes tree64
    putStrLn $ show $ layout tree64
--    putStrLn $ show $ countNodes (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)))
    
