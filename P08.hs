--
-- Problem 08

compress :: Eq a => [a] -> [a]
compress [] = []
compress (a:as) = a : compress (filter (/= a) as)


main :: IO()
main = do
    putStrLn $ show $ compress "aaaabccaadeeee"
