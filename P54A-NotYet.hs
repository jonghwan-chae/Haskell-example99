--
-- Problem 54A :: Check whether a given term represents a binary tree 

{-  
Example in Lisp: 
* (istree (a (b nil nil) nil))
T
* (istree (a (b nil nil)))
NIL

Non-solution: 
Haskell's type system ensures that all terms of type Tree a are binary trees: 
it is just not possible to construct an invalid tree with this type. 
Hence, it is redundant to introduce a predicate to check this property: it would always return True. 
-}


data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

