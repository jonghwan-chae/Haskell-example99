--
-- Problem 15

import Data.List

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (a:as) n = (replicate n a) ++ repli as n

main :: IO()
main = do
    putStrLn $ show $ repli "abc" 3
    -- ret : "aaabbbccc"
