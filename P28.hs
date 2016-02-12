--
-- Problem 28

import Data.List
import Control.Monad

lsort :: [String] -> [String]
lsort []     = []
lsort (a:as) = ins a (lsort as)

ins :: String -> [String] -> [String]
ins item []         = [item]
ins item s@(a:as)
    | length item <= length a = item : s
    | otherwise               = a : ins item as


lfsort :: [String] -> [String]
lfsort []     = []
lfsort strs = map (\x -> snd x) $ insFLen $ map (\x -> (freq x strs, x)) strs
    where freq s str = length $ filter (\x -> length x == length s) str

insFLen :: [(Int, String)] -> [(Int, String)]
insFLen []     = []
insFLen (a:as) = insF a (insFLen as)

insF :: (Int, String) -> [(Int, String)] -> [(Int, String)]
insF item []         = [item]
insF item s@(a:as)
    | fst item <= fst a = item : s
    | otherwise         = a : insF item as

main :: IO()
main = do
    putStrLn $ show $ lsort  ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    -- ret : ["o","de","de","mn","abc","fgh","ijkl"]
    putStrLn $ show $ lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    -- ret : ["ijkl","o","abc","fgh","de","de","mn"]
