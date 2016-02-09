--
-- Problem 04

myLength :: [a] -> Int
myLength []     = 0
myLength (a:as) = 1 + myLength (as)

main :: IO()
main = do
    putStrLn $ show $ myLength [123, 456, 789]
    putStrLn $ show $ myLength "Hello, world!"
    