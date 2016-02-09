--
-- Problem 39

import Data.List

primtsNum :: [Int] -> [Int]
primtsNum []     = []
primtsNum (a:as) = a : primtsNum ( filter (\x -> (x `mod` a) /= 0) as )

primeR :: Int -> Int -> [Int]
primeR n1 n2 = filter (\x -> x >= n1) $ primtsNum [2..n2]

main :: IO()
main = do
    putStrLn $ show $ primeR 10 20
    -- ret : [11,13,17,19]


