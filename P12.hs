--
-- Problem 12

import Data.List

data Multiple  = Multiple Int Char | Single Char
    deriving Show

decodeModified :: [Multiple] -> String
decodeModified [] = []
decodeModified (a:as) = case a of
    (Single ch)     -> [ch] ++ decodeModified(as)
    (Multiple n ch) -> (replicate n ch) ++ decodeModified(as)

main :: IO()
main = do
    putStrLn $ show $ decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
