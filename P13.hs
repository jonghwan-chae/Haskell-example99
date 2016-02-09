--
-- Problem 13

import Data.List

data DataItem  = Multiple Int Char | Single Char
    deriving Show

encodeDirect :: String -> [DataItem]
encodeDirect [] = []
encodeDirect (a:as) 
    | (length as) == 0  = [Single a]
    | a /= (head as)    = [Single a] ++ rest
    | a == (head as)    = (encodingAdd.head) (rest) : tail(rest)
    where
        rest =  encodeDirect (as)
        encodingAdd item = case item of
            Multiple n ch  -> Multiple (n+1) ch
            Single ch      -> Multiple 2 ch

main :: IO()
main = do
    putStrLn $ show $ encodeDirect "aaaabccaadeeee"
    -- ret : [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
