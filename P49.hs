--
-- Problem 49

import Control.Monad
import Data.List

change_code :: Int -> String -> String
change_code n code = (take n code) ++ [change ((!!) code n)] ++ (drop (n+1) code)
            
        where change code = case code of
                    '1' -> '0'
                    '0' -> '1'

gray :: Int -> [String]
gray n = gray_perm n 0 (replicate n '0')

gray_perm :: Int -> Int -> String -> [String]
gray_perm num cur base 
    | cur == num   = [base]
    | otherwise     = let pre = gray_perm num (cur+1) base in pre ++ (gray_perm num (cur+1) $ change_code cur (last pre))


    
