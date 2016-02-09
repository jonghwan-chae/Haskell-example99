--
-- Problem 58 :: Generate-and-test paradigm 

{-  
Apply the generate-and-test paradigm to construct all symmetric, 
completely balanced binary trees with a given number of nodes. 
-}

import Control.Monad

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

--
-- module : make all complete binary tree 

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

makeTree :: Int -> Int -> [Int] -> Tree Char
makeTree n c llist
    | n < 1     = Empty
    | n == 1    = case (llist!!c) of
                    1 -> Branch ('x') Empty Empty  
                    _ -> Empty          
    | otherwise = Branch ('x') (makeTree (n-1) (c*2) llist) (makeTree (n-1) (c*2 + 1) llist)

cbalTree :: Int -> [Tree Char]   
cbalTree n = do 
    item <- cbalLast level rest
    [makeTree level 0 item]
    where
        level = cbalLevel n
        rest = n - (( 2 ^ (level-1)) - 1)

--
-- module : check synmetric
preOrder :: Eq a => Tree a -> String
preOrder tree = case tree of
                    Empty          ->  "E"
                    Branch _ l r -> "X" ++ (preOrder l) ++ (preOrder r)

postOrder :: Eq a => Tree a -> String
postOrder tree = case tree of
                    Empty          ->  "E"
                    Branch _ l r -> (postOrder l) ++ (postOrder r) ++ "X"

symmetric  :: Eq a => Tree a -> Bool
symmetric tree  = case tree of
                    Empty          ->  True
                    Branch _ l r -> ((==) (preOrder l) (reverse.postOrder $ r)) 
                                    && ((==) (postOrder l) (reverse.preOrder $ r))

--
--


symCbalTrees :: Int -> [Tree Char]  
symCbalTrees n = do
    tree <- cbalTree n
    if (symmetric tree) then [tree] else []    


{-
Example in Haskell: 

*Main> symCbalTrees 5
[Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
-}
