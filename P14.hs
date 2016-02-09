--
-- Problem 14

import Data.List

dupli :: [a] -> [a]
dupli [] = []
dupli (a:as) = a:a:dupli(as)

main :: IO()
main = do
    putStrLn $ show $ dupli [1, 2, 3]
    -- ret : [1,1,2,2,3,3]
