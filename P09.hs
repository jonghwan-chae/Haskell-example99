--
-- Problem 09

import Data.List

pack :: Eq a => [a] -> [[a]]
pack s = group s

main :: IO()
main = do
    putStrLn $ show $ group ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
