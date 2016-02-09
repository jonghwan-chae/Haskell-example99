--
-- Problem 27

import Data.List
import System.Random
import Control.Monad

groups :: [Int] -> [String] -> [[[String]]]
groups cnts strs = do
    let n = head cnts   
    let en = (length strs) - n
    check <- [0 .. en]
    first <- combi (n-1) strs [check]

    let rest1 = [x | x <- strs, (x `elem` first) == False]
    let n2 = head (tail cnts)
    let en2 = (length rest1) - n2
    check2 <- [0 .. en2]
    second <- combi (n2 - 1) rest1 [check2]
    let rest2 = [x | x <- rest1, (x `elem` second) == False]

    [[first] ++ [second] ++ [rest2]]



combi :: Int -> [String] -> [Int] -> [[String]]
combi n strs list
    | n == 0                    = [ [strs!!p | p <- list] ]            
    | (last list) >= (len - n)  = []
    | otherwise                 = do 
            let st = (last list) + 1
            check <- [st .. (len - 1)]
            combi (n - 1) strs (list ++ [check])
    where len = length strs

main :: IO()
main = do
    let ret = groups [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    putStrLn $ show ret
    putStrLn $ show (length ret)
    -- ret : [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...] (altogether 756 solutions)

    -- groups [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
