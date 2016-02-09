--
-- Problem 62B :: Collect the nodes at a given level in a list 

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

atLevel :: Eq a => Tree a -> Int -> [a] -- tree4 2
atLevel Empty _                  = []
atLevel _ 0                      = []
atLevel (Branch v _ _) 1 = [v]
atLevel (Branch _ l r) lev       = (atLevel l (lev-1)) ++ (atLevel r (lev-1))

-- ProBlem 62
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

