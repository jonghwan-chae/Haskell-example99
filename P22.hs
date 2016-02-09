--
-- Problem 22

import Data.List

range :: Int -> Int -> [Int]
range st en 
    | st > en   = []
    | otherwise = st : range (st+1) en 

main :: IO()
main = do
    putStrLn $ show $ range 4 9
    -- ret : [4,5,6,7,8,9]
