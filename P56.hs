--
-- Problem 56 :: Symmetric binary trees

{-  
Let us call a binary tree symmetric if you can draw a vertical line through the root node 
and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 
to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first 
to check whether one tree is the mirror image of another. 
We are only interested in the structure, not in the contents of the nodes.
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

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


{-
Example in Haskell: 

*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True
-}

