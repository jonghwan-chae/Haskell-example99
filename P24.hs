--
-- Problem 24

import Data.List
import System.Random
import Control.Monad 



diff_select :: Int -> Int -> IO [Int]
diff_select n m = do 
    rnds <- replicateM (m*n) $ randomRIO (1, m)     --getStdRandom $ randomR (1, m)
    let list = take n $ check_diff rnds in return list
    where check_diff list@(a:as) = a : (filter (/= a) (check_diff as) )



diff_select2 :: Int -> Int -> IO [Int]
diff_select2 n to = diff_select' n [1..to]
 
diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0,(length xs)-1)
                       let remaining = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remaining
                       return ((xs!!r) : rest)

main :: IO()
main = do
    list <- diff_select 10 49
    putStrLn $ show list  
    -- ret : [23,1,17,33,21,37]

