--
-- Problem 19

import Data.List

rotate :: [a] -> Int -> [a]
rotate s n 
    | n >= 0  = drop n s ++ take n s
    | n < 0   = drop (len + n) s ++ take (len + n) s
    where len = length s

main :: IO()
main = do
    putStrLn $ show $ rotate ['a','b','c','d','e','f','g','h'] 3
    -- ret : "defghabc"
    putStrLn $ show $ rotate ['a','b','c','d','e','f','g','h'] (-2)
    -- ret : "ghabcdef"
