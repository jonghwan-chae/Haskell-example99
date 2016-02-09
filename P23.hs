--
-- Problem 23

import Data.List
import System.Random
import Control.Monad (replicateM)

rnd_select2 :: [a] -> Int -> IO [a]
rnd_select2 [] _ = return []
rnd_select2 l  n 
    | n < 0 = error "N must be greater than zero."
    | otherwise = do pos <- replicateM n $ getStdRandom $ randomR (0, (length l)-1)
                     return [l!!p | p <- pos]

rnd_select :: [a] -> Int -> [a]
rnd_select s n = (!!) s s1 : (!!) s s2 : (!!) s s3  : []
    where 
        len = length s
        (s1, g1) = randomR (0, len - 1) (mkStdGen 0)
        (s2, g2) = randomR (0, len - 1) g1
        (s3, g3) = randomR (0, len - 1) g2

main :: IO()
main = do
    putStrLn $ show $ rnd_select "abcdefgh" 3
    -- ret : eda
    rnd_select2 "abcdefgh" 3 >>= putStrLn
    -- ret : eda
