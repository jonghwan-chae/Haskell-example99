--
-- Problem 61 :: Count the leaves of a binary tree 

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

countLeaves :: Eq a => Tree a -> Int
countLeaves Empty                   = 0
countLeaves (Branch _ Empty Empty)  = 1
countLeaves (Branch _ l r)          = (countLeaves l) + (countLeaves r)



