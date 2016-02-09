--
-- Problem 33

import Data.List

coprime :: Int -> Int -> Bool
coprime m n = (==) (length $ coprimes m n) 1

getDivisor :: Int -> [Int]
getDivisor n = filter (\x -> n `mod` x == 0) [1..n] 

coprimes :: Int -> Int -> [Int]
coprimes m n = [x | x <- a, (x `elem` b) == True]
    where
        a = getDivisor m
        b = getDivisor n

main :: IO()
main = do
    putStrLn $ show $ coprime 35 64
    -- ret : True
