--
-- Problem 66::  a layout algorithm for drawing the tree in very compact layout while maintaining a certain symmetry in every node.
 
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

layout :: Tree Char -> Tree (Char, (Int, Int))
layout Empty = Empty
layout tree = makeTree checkTree 1 ((startpos checkTree) - 1)
    where
        checkTree = layout_cnt tree; 
        startpos tree = case (tree) of
                            Empty -> 0
                            (Branch var l _) -> (fst $ snd var) + startpos l

makeTree :: Tree (Char, (Int, Int)) -> Int -> Int -> Tree (Char, (Int, Int))
makeTree Empty _ _ = Empty
makeTree (Branch var l r) lev pos = Branch (fst var, (pos, lev)) (makeTree l (lev + 1) (pos - tab))
                                                                 (makeTree r (lev + 1) (pos + tab))
    where tab = min (fst $ snd var) (snd $ snd var)


layout_cnt :: Tree Char -> Tree (Char, (Int, Int))
layout_cnt Empty = Empty
-- layout (Branch var Empty Empty) = Branch (var, (0, 0)) Empty Empty
-- layout (Branch var Empty Empty) = Branch (var, (0, 0)) Empty Empty
layout_cnt (Branch var l r) = Branch (var, (l2, r2)) ltree rtree
    where
        ltree = layout_cnt l
        rtree = layout_cnt r
        lr = case ltree of
                Empty            -> (('.', (0,0)), Empty)
                (Branch var l r) -> (var, r)
        rr = case rtree of
                Empty            -> (('.', (0,0)), Empty)
                (Branch var l r) -> (var, l)
        l2 = case (snd lr) of 
                Empty            -> 1
                (Branch var l r) -> 1 + (snd $ snd var) 
        r2 = case (snd rr) of 
                Empty            -> 1
                (Branch var l r) -> 1 + (fst $ snd var)

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
-}
--    putStrLn $ show $ countNodes (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)))
    
