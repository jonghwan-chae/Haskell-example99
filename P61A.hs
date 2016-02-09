--
-- Problem 61A :: Collect the leaves of a binary tree in a list 

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


leaves :: Eq a => Tree a -> [a]
leaves Empty                  = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch v l r)         = (leaves l)  ++ (leaves r)

-- ProBlem 61
countLeaves :: Eq a => Tree a -> Int
countLeaves Empty                   = 0
countLeaves (Branch _ Empty Empty)  = 1
countLeaves (Branch _ l r)          = (countLeaves l) + (countLeaves r)


