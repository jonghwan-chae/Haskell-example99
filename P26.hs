--
-- Problem 26

import Data.List
import System.Random
import Control.Monad

combinations :: Int -> String -> [String]
combinations n str = do
    let en = (length str) - n
    check <- [0 .. en]
    combi (n-1) str [check]

combi :: Int -> String -> [Int] -> [String]
combi n str list 
    | n == 0                    = [[str!!p | p <- list]]
    | (last list) >= (len - n)  = []
    | otherwise                 = do 
            let st = (last list) + 1
            check <- [st .. (len - 1)]
            combi (n-1) str (list ++ [check])
    where len = length str

main :: IO()
main = do
    putStrLn $ show $  combinations 3 "abcdef"  
    -- ret : ["abc","abd","abe",...]

