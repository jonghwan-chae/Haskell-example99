--
-- Problem 57 :: Binary search trees (dictionaries) 

{-  
Use the predicate add/3, developed in chapter 4 of the course, 
to write a predicate to construct a binary search tree from a list of integer numbers. 
-}

import Control.Monad.State
import Data.List

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

addItem :: Ord a => a -> Tree a -> Tree a
addItem n tree = case (tree) of
                    Empty          -> Branch n Empty Empty
                    Branch val l r -> if (n < val) then Branch val (addItem n l) r
                                                   else Branch val l (addItem n r) 

construct :: [Int] -> Tree Int 
construct items = foldl (\b a -> addItem a b) Empty items


 
{-
Example in Haskell: 

*Main> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
*Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True
*Main> symmetric . construct $ [3, 2, 5, 7, 1]
True
-}
