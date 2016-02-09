--
-- Problem 63 :: Construct a complete binary tree

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


completeBinaryTree :: Int -> Tree Char
completeBinaryTree n 
    | n <= 0    = Empty
    | otherwise = Branch 'x' (completeBinaryTree left) (completeBinaryTree right) 
    where 
        level = getLevel n
        defVal = if (level >= 2) then 2 ^ (level-2) else 1
        rest  = n - 1 - 2 * (defVal - 1)
        left  = if (rest > defVal) then 2 * defVal - 1 else defVal + rest - 1
        right = if (rest > defVal) then rest - 1 else defVal - 1


getLevel :: Int -> Int
getLevel n = level n 1
    where level n c = if (n >= 2 ^ c) then level n (c+1) else c


-- ProBlem 62B
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
