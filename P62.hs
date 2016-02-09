--
-- Problem 62 :: Collect the internal nodes of a binary tree in a list 

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

internals  :: Eq a => Tree a -> [a]
internals  Empty                   = []
internals  (Branch v Empty Empty)  = []
internals  (Branch v l r)          = [v] ++ (internals l)  ++ (internals r)

-- ProBlem 61A
leaves :: Eq a => Tree a -> [a]
leaves Empty                  = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch v l r)         = (leaves l)  ++ (leaves r)

-- ProBlem 61
countLeaves :: Eq a => Tree a -> Int
countLeaves Empty                   = 0
countLeaves (Branch _ Empty Empty)  = 1
countLeaves (Branch _ l r)          = (countLeaves l) + (countLeaves r)

