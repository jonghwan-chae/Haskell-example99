--
-- Problem 01

myList :: [a] -> Int
myList [] = 0
myList (a:as) = 1 + myList(as) 

main :: IO()
main = do
    putStrLn $ show $ myList [1,2,3,4]
    putStrLn $ show $ myList ['x', 'y', 'z']