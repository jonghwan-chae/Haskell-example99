--
-- Problem 20

import Data.List

removeAt :: Int -> [a] -> (a,[a])
removeAt n s = ((!!) s (n-1), take (n-1) s ++ drop n s) 

main :: IO()
main = do
    putStrLn $ show $ removeAt 2 "abcd"
    -- ret : ('b',"acd")
