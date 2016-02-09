--
-- Problem 18

import Data.List

slice :: [a] -> Int -> Int -> [a]
slice s st en = take (en-st+1) $ drop (st-1) s 

main :: IO()
main = do
    putStrLn $ show $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    -- ret : "cdefg"
