--
-- Problem 28

import Data.List
import System.Random
import Control.Monad

lsort :: [String] -> [String]
lsort []     = []
lsort (a:as) = ins a (lsort as)

insLen :: String -> [String] -> [String]
insLen item []         = [item]
insLen item s@(a:as)
    | length item <= length a = item : s
    | otherwise              = a : ins item as

lfsort :: [String] -> [String]
lfsort []     = []
lfsort (a:as) = insFreq a (lsort as)

main :: IO()
main = do
    putStrLn $ show $ lsort  ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    -- ret : ["o","de","de","mn","abc","fgh","ijkl"]
    putStrLn $ show $ lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    -- ret : ["ijkl","o","abc","fgh","de","de","mn"]
