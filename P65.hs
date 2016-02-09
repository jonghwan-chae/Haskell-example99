--
-- Problem 65::  a layout algorithm for drawing the tree in a rectangular grid, to determine the position of each node
 
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

layout :: Eq a => Tree a -> Tree (a, (Int, Int))
layout tree = makeTree tree 1 startGap firstPos
    where 
        makeTree t lev g p = case t of
                Empty           -> Empty
                (Branch v l r)  -> Branch (v, (p, lev)) (makeTree l (lev+1) (div g 2) (p - g)) 
                                                        (makeTree r (lev+1) (div g 2) (p + g))
        startGap = gaps level
        gaps n = if (n <= 2) then 1 
                             else 2 * gaps (n-1)
        firstPos = getpos startGap lefts
        getpos n l = if (l <= 1) then 1 
                                 else n + getpos (div n 2) (l-1)
        level = getLevel tree
        lefts = getLefts tree

getLefts :: Eq a => Tree a -> Int
getLefts Empty = 0
getLefts (Branch _ l _) = 1 + getLefts l

getLevel :: Eq a => Tree a -> Int
getLevel Empty = 0
getLevel (Branch a l r) = 1 + max (getLevel l) (getLevel r)


countNodes :: Eq a => Tree a -> Int
countNodes Empty          = 0
countNodes (Branch _ l r) = 1 + (countNodes l) + (countNodes r)

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


main :: IO()
main = do
    putStrLn $ show $ countNodes tree65
    putStrLn $ show $ layout tree65
--    putStrLn $ show $ countNodes (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)))
    
