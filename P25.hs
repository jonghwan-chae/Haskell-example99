--
-- Problem 25

import Data.List
import System.Random
import Control.Monad 

rnd_permu :: String -> IO String
rnd_permu str = do
    let n = length str
    pos <- diff_select' (n) [0..(n-1)]
    return [str!!p | p <- pos]
 
diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0, (length xs)-1)
                       let remaining = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remaining
                       return ((xs!!r) : rest)

main :: IO()
main = do
    ret <- rnd_permu "abcdef"
    putStrLn $ show ret  
    -- ret : [23,1,17,33,21,37]

