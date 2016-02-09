--
-- Problem 02

myButList :: [a] -> Maybe a
myButList s 
    | len <= 1  = Nothing
    | otherwise = Just $ (!!) s (len-2) 
    where len = length s

main :: IO()
main = do
    putStrLn $ show $ myButList [1,2,3,4]
    putStrLn $ show $ myButList ['x', 'y', 'z']
