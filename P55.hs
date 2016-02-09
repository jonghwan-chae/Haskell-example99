--
-- Problem 55 :: Construct completely balanced binary trees 

{-  
Example in Lisp: 
* (istree (a (b nil nil) nil))
T
* (istree (a (b nil nil)))
NIL

Non-solution: 
Haskell's type system ensures that all terms of type Tree a are binary trees: 
it is just not possible to construct an invalid tree with this type. 
Hence, it is redundant to introduce a predicate to check this property: it would always return True. 
-}

import Control.Monad

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalLast :: Int -> Int -> [[Int]]
cbalLast n k = do
    check <- replicateM (2 ^ (n-1)) [0, 1]
    let cnt = foldr (+) 0 check 
    if (cnt == k) then [check] else []

cbalLevel :: Int -> Int
cbalLevel 0 = 0
cbalLevel n = check n 1
    where check n l 
            | n < ((^) 2 l)   = l
            | otherwise     = check n (l+1)

cbalTree :: Int -> [Tree Char]   
cbalTree n = do 
    item <- cbalLast level rest
    [makeTree level 0 item]
    where
        level = cbalLevel n
        rest = n - (( 2 ^ (level-1)) - 1)

makeTree :: Int -> Int -> [Int] -> Tree Char
makeTree n c llist
    | n < 1     = Empty
    | n == 1    = case (llist!!c) of
                    1 -> Branch ('x') Empty Empty  
                    _ -> Empty          
    | otherwise = Branch ('x') (makeTree (n-1) (c*2) llist) (makeTree (n-1) (c*2 + 1) llist)

{-
Example in Haskell
whitespace and "comment diagrams" added for clarity and exposition: 

*Main> cbalTree 4
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)),
 
-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty),
 
-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)) 
           (Branch 'x' Empty Empty),
 
-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty) 
           (Branch 'x' Empty Empty)
]
-}
