--
-- Problem 06

import Data.List

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome s  = and $ zipWith (==) s (reverse s)

main :: IO()
main = do
    putStrLn $ show $ isPalindrome [1,2,3]
    putStrLn $ show $ isPalindrome "madamimadam"
    putStrLn $ show $ isPalindrome [1,2,4,8,16,8,4,2,1]

