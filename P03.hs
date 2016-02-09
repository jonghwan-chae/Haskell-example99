--
-- Problem 03

elementAt :: [a] -> Int -> a
elementAt s n = (!!) s (n-1)

main :: IO()
main = do
    putStrLn $ show $ elementAt [1,2,3,4] 2
    putStrLn $ show $ elementAt "haskell" 5
