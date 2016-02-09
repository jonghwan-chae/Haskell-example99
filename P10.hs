--
-- Problem 10

import Data.List

encode :: (Eq a) => [a] -> [(Int, a)]
encode s = map (\x -> (length x, head x)) $ group s

main :: IO()
main = do
    putStrLn $ show $ encode "aaaabccaadeeee"
