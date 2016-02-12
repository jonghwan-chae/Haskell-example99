--
-- Problem 01

myList :: [a] -> a
myList (a:[]) = a
myList (a:as) = myList(as) 

main :: IO()
main = do
    putStrLn $ show $ myList [1,2,3,4]
    putStrLn $ show $ myList ['x', 'y', 'z']