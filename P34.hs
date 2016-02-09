--
-- Problem 34

import Data.List

getDivisor :: Int -> [Int]
getDivisor n = filter (\x -> n `mod` x == 0) [1..n] 

coprimes :: Int -> Int -> [Int]
coprimes m n = [x | x <- a, (x `elem` b) == True]
    where
        a = getDivisor m
        b = getDivisor n

coprime :: Int -> Int -> Bool
coprime m n = (==) (length $ coprimes m n) 1

totient_phi  :: Int -> [Int]
totient_phi  n = [x | x <- [1 .. n], (coprime n x)]

totient :: Int -> Int
totient n = (length.totient_phi) n

main :: IO()
main = do
    putStrLn $ show $ totient  10
    -- ret : True
