--
-- Problem 16

import Data.List

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery s n 
    | length s < 3  = s 
    | length s >= 3 = drop s n
    where drop (a:b:c:as) n = a:b:dropEvery as n

main :: IO()
main = do
    putStrLn $ show $ dropEvery "abcdefghik" 3
    -- ret : "abdeghk"
