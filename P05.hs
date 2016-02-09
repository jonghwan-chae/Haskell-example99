--
-- Problem 05

myReverse  :: [a] -> [a]
myReverse  []     = []
myReverse  (a:as) =  myReverse (as) ++ [a]

main :: IO()
main = do
    putStrLn $ show $ myReverse "A man, a plan, a canal, panama!"
    putStrLn $ show $ myReverse [1,2,3,4]
    