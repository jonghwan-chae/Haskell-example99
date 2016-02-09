--
-- Problem 32

import Data.List

getGCD :: Int -> Int -> Int
getGCD m n
    | r == 0     = b
    | otherwise  = getGCD b r
    where
        a = abs m
        b = abs n 
        r = (mod) a b

main :: IO()
main = do
    putStrLn $ show $ getGCD 100  (-30)
    -- ret : True
