--
-- Problem 21

import Data.List

insertAt :: a -> [a] -> Int -> [a]
insertAt ch s n = take (n-1) s ++ [ch] ++ drop (n-1) s

main :: IO()
main = do
    putStrLn $ show $ insertAt 'X' "abcd" 2
    -- ret : "aXbcd"
