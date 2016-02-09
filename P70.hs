--
-- Problem 71:: Tree construction from a node string.

import Data.List 

data Tree a = Node a [Tree a]
        deriving (Eq, Show)
{-
A multiway tree is composed of a root element and a (possibly empty) set of successors 
which are multiway trees themselves. A multiway tree is never empty. 
The set of successor trees is sometimes called a forest. 
-}

stringToTree :: String -> Tree Char
stringToTree str = case (length str) of
    2 -> Node (head str) []
    _ -> Node (head str) $ map stringToTree check
    where 
        check = checkStr [] (checkSubstr str)
        checkSubstr str = do  
            temp <- tail $ inits $ tail str
            let count = foldr (\a b -> if (a == '^') then (b+1) else (b-1)) 0 temp
            if (count == 0) then [temp] else []
        checkStr p list = case list of
            (a:[]) -> [drop (length p) a]
            (a:as) -> (drop (length p) a) : (checkStr a as)


treeToString :: Tree Char -> String 
treeToString tree = case tree of
    (Node a []) -> [a] ++ "^"
    (Node a ts) -> [a] ++ (foldr ((++).treeToString) [] ts) ++ "^"


{-
Example in Haskell: 

Tree> stringToTree "afg^^c^bd^e^^^"
Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
 
Tree> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
"afg^^c^bd^e^^^"

-}


tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]


{-
main :: IO()
main = do
    putStrLn $ show $ countNodes tree65
    putStrLn $ show $ layout tree65
--    putStrLn $ show $ countNodes (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)))
-}    
