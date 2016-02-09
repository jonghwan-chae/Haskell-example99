--
-- Problem 36

import Data.List

primes :: Int -> Int -> [Int]
primes n p 
    | n == 1          = []
    | 0 == (mod) n p  = p : primes (div n p) p
    | otherwise       = primes n (p+1)      

primeFactors :: Int -> [Int]
primeFactors n = primes n 2

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = do
    plist <- (group.primeFactors) n
    [(head plist, length plist)]

prime_factors_mult2 :: Int -> [(Int, Int)]
prime_factors_mult2 n =  map (\x -> (head x, length x)) $ (group.primeFactors) n

main :: IO()
main = do
    putStrLn $ show $ prime_factors_mult 315
    -- ret : [(3,2),(5,1),(7,1)]

    putStrLn $ show $ prime_factors_mult2 315

