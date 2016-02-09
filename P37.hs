--
-- Problem 37

import Data.List

primes :: Int -> Int -> [Int]
primes n p 
    | n == 1          = []
    | 0 == (mod) n p  = p : primes (div n p) p
    | otherwise       = primes n (p+1)      

primeFactors :: Int -> [Int]
primeFactors n = primes n 2

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n =  map (\x -> (head x, length x)) $ (group.primeFactors) n

phi :: Int -> Int
phi n = foldr (\a b -> b * (fst a - 1) * (pow (fst a) (snd a - 1)) ) 1 $ prime_factors_mult n
    where pow n p = if (p == 0) then 1 else n * pow n (p-1)

main :: IO()
main = do
    putStrLn $ show $ prime_factors_mult 315
    putStrLn $ show $ phi 315
    -- ret : [(3,2),(5,1),(7,1)]


