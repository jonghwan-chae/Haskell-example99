--
-- Problem 31

import Data.List

isPrime :: Int -> Bool
isPrime n = if 2 == (length . filter (\x -> n `mod` x == 0)) [1..n] 
            then True else False

main :: IO()
main = do
    putStrLn $ show $ isPrime 7
    -- ret : True
    putStrLn $ show $ isPrime 2
    putStrLn $ show $ isPrime 3
    putStrLn $ show $ isPrime 4
    putStrLn $ show $ isPrime 5
    putStrLn $ show $ isPrime 6

