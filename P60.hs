--
-- Problem 60 :: Construct height-balanced binary trees with a given number of nodes 

{-  
Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain? 

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. 
Try to find a recursive statement and turn it into a function minNodes that returns the minimum number 
of nodes in a height-balanced binary tree of height H. On the other hand, we might ask: what is the maximum 
height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this. 

Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. 
Find out how many height-balanced trees exist for N = 15. 
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

countNodes :: Eq a => Tree a -> Int
countNodes tree = case tree of
    Empty         -> 0
    Branch _ l r  -> 1 + (countNodes l) + (countNodes r)

minLevel :: Int -> Int
minLevel n = countLevel n 0 
    where level = log (1 + fromIntegral n) / log 2
          countLevel n l = if (level <= fromIntegral l) then l else countLevel n (l+1) 

maxLevel :: Int -> Int
maxLevel n 
    | n <= 2    =  n
    | otherwise = countLevel n 2 [4,1,2]
    where countLevel n level s  = if (n < head s) then level
                                                  else (countLevel n (level+1) [sum s, s!!2, s!!1+s!!2])

hbalTreeNodes :: Eq a => a -> Int -> [Tree a]
hbalTreeNodes value n = filter (\x -> n == countNodes x) $ hbalTreeNode_inner value n
    where hbalTreeNode_inner value n = do
                level <- [(minLevel n) .. (maxLevel n)]
                hbalTree value level


{-
Example in Haskell: 

*Main> length $ hbalTreeNodes 'x' 15
1553
*Main> map (hbalTreeNodes 'x') [0..3]
[[Empty],
 [Branch 'x' Empty Empty],
 [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
 [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
-}