--
-- Problem 40

import Data.List

primtsNum :: [Int] -> [Int]
primtsNum []     = []
primtsNum (a:as) = a : primtsNum ( filter (\x -> (x `mod` a) /= 0) as )

goldbach :: Int -> [(Int, Int)]
goldbach n = let ps = primtsNum [2..n] in [(x, n-x) | x <- ps, x * 2 < n, elem (n-x) ps ] 

main :: IO()
main = do
    putStrLn $ show $ goldbach 28
    -- ret : (5, 23)


