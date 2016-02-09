--
-- Problem 11

import Data.List

data Multiple  = Multiple Int Char | Single Char
    deriving Show

encodeModified :: String -> [Multiple]
encodeModified s = map (encoding) $ group s
    where encoding str = if (length str) == 1 
                            then Single (head str)
                            else Multiple (length str) (head str)

main :: IO()
main = do
    putStrLn $ show $ encodeModified "aaaabccaadeeee"
