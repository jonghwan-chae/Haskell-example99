--
-- Problem 46

import Data.List

and' :: Bool -> Bool -> Bool
and' p1 p2 = case (p1, p2) of
                (True, True) -> True
                (True, False) -> False
                (False, True) -> False
                (False, False) -> False

nand' :: Bool -> Bool -> Bool
nand' p1 p2 = case (p1, p2) of
                (True, True) -> False
                (True, False) -> True
                (False, True) -> True
                (False, False) -> True

or' :: Bool -> Bool -> Bool
or' p1 p2 = case (p1, p2) of
                (True, True) -> True
                (True, False) -> True
                (False, True) -> True
                (False, False) -> False

nor' :: Bool -> Bool -> Bool
nor' p1 p2 = case (p1, p2) of
                (True, True) -> False
                (True, False) -> False
                (False, True) -> False
                (False, False) -> True

xor' :: Bool -> Bool -> Bool
xor' p1 p2 = case (p1, p2) of
                (True, True) -> False
                (True, False) -> True
                (False, True) -> True
                (False, False) -> False

equ' :: Bool -> Bool -> Bool
equ' p1 p2 = case (p1, p2) of
                (True, True) -> True
                (True, False) -> False
                (False, True) -> False
                (False, False) -> True

-- IMPL is True if a implies b, equivalent to (not a) or (b)
impl' :: Bool -> Bool -> Bool
impl' p1 p2 = case (p1, p2) of
                (True, True) -> True
                (True, False) -> False
                (False, True) -> True
                (False, False) -> True

table2 :: (Bool -> Bool -> Bool) -> IO()
table2 f = do
    putStr "True True " 
    putStrLn $ show $ f True True  
    putStr "True False " 
    putStrLn $ show $ f True False  
    putStr "False True " 
    putStrLn $ show $ f False True  
    putStr "False False " 
    putStrLn $ show $ f False False  
    


