--
-- Problem 59 :: Construct height-balanced binary trees 

{-  
In a height-balanced binary tree, the following property holds for every node: The height of its left subtree 
and the height of its right subtree are almost equal, which means their difference is not greater than one. 

Construct a list of all height-balanced binary trees with the given element and the given maximum height. 
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

hbalTree :: Eq a => a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree val 1 = [Branch val Empty Empty]
hbalTree val lev = do 
    levSet <- [(lev-1, lev-2), (lev-2, lev-1), (lev-1, lev-1)]
    hbal1 <- hbalTree val $ fst levSet
    hbal2 <- hbalTree val $ snd levSet
    [Branch val hbal1 hbal2]

hbalTree2 :: Eq a => a -> Int -> [Tree a]
hbalTree2 _ 0 = [Empty]
hbalTree2 val 1 = [Branch val Empty Empty]
hbalTree2 val lev = (hbal_in val (lev-1) (lev-2) ) ++ (hbal_in val (lev-1) (lev-2)) ++ (hbal_in val (lev-1) (lev-1))
    where hbal_in val ll rl = do
            hbal1 <- hbalTree val ll
            hbal2 <- hbalTree val rl
            [Branch val hbal1 hbal2]
{-
Example in Haskell: 

*Main> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
-}

