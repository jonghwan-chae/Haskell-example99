--
-- Problem 17

import Data.List

split :: [a] -> Int -> ([a], [a])
split s n 
    | length s <= n = (s, [])
    | length s > n  = (take n s, drop n s)

main :: IO()
main = do
    putStrLn $ show $ split "abcdefghik" 3
    -- ret : ("abc", "defghik")
