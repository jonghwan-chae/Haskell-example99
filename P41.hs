--
-- Problem 41

import Data.List

primtsNum :: [Int] -> [Int]
primtsNum []     = []
primtsNum (a:as) = a : primtsNum ( filter (\x -> (x `mod` a) /= 0) as )

goldbach :: Int -> (Int, Int)
goldbach n = let ps = primtsNum [2..n] in head [(x, n-x) | x <- ps, x * 2 <= n, elem (n-x) ps ] 

goldbach_list :: Int -> Int -> [(Int, Int)]
goldbach_list st en = map (goldbach) $ filter (\x -> 0 == (mod) x 2) [st..en]

goldbach_list' :: Int -> Int -> Int -> [(Int, Int)]
goldbach_list' st en min = filter (\n -> (fst n) >= min) $ goldbach_list st en

main :: IO()
main = do
    putStrLn $ show $ goldbach_list 9 20
    -- ret : [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]

    putStrLn $ show $ goldbach_list' 4 2000 50
    -- ret : [(73,919),(61,1321),(67,1789),(61,1867)]
