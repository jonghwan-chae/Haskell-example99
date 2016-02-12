--
-- Problem 03

elementAt :: [a] -> Int -> Maybe a
elementAt (a:as) n 
    | n <= 0    = Nothing
    | n == 1    = Just a
    | otherwise = elementAt as (n-1)

main :: IO()
main = do
    putStrLn $ show $ elementAt [1,2,3,4] 2
    putStrLn $ show $ elementAt "haskell" 5
