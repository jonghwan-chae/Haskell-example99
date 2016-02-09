--
-- Problem 35

import Data.List

primes :: Int -> Int -> [Int]
primes n p 
    | n == 1          = []
    | 0 == (mod) n p  = p : primes (div n p) p
    | otherwise       = primes n (p+1)      

primeFactors :: Int -> [Int]
primeFactors n = primes n 2

main :: IO()
main = do
    putStrLn $ show $ primeFactors 315
    -- ret : True
