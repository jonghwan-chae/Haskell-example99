--
-- Problem 67A::  a layout algorithm for drawing the tree in very compact layout while maintaining a certain symmetry in every node.

import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

stringToTree :: String -> Maybe (Tree Char)
stringToTree []  = return Empty
stringToTree (a:[]) = return (Branch a Empty Empty) 
stringToTree str@(a:as) = do 
    hstat <- if (1 > length stat) then Nothing else return (head stat)
    left <- (stringToTree $ fst hstat) 
    right <- (stringToTree $ snd hstat)
    if (stat == []) then Nothing
                    else return (Branch a left right)
    where stat = checkString str

checkString :: String -> [(String, String)]
checkString str = do
    let nstr = drop 2 $ take (length str - 1) str
    cnt <- commalist nstr
    let chk = take cnt nstr
    let sts = (length $ elemIndices '(' chk) - (length $ elemIndices ')' chk)
    if (sts == 0) then [(take cnt nstr, drop (cnt+1) nstr)] else []
    where commalist s = elemIndices ',' s


--
-- print Modules

tree65 = Branch 'n'
            (Branch 'k'
                (Branch 'c'
                    (Branch 'a' Empty Empty)
                    (Branch 'e'
                        (Branch 'd' Empty Empty)
                        (Branch 'g' Empty Empty)
                    )
                )
                (Branch 'm' Empty Empty)
            )
            (Branch 'u'
                (Branch 'p'
                    Empty
                    (Branch 'q' Empty Empty)
                )
                Empty
            )

{-
main :: IO()
main = do
    putStrLn $ show $ countNodes tree65
    putStrLn $ show $ layout tree65
-}
--    putStrLn $ show $ countNodes (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)))
    
