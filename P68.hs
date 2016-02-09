--
-- Problem 68::  Preorder and inorder sequences of binary trees, 
 
import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

--
-- General Tree Expression  <==>  Tree
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
-- Preorder, Inorder  <==> Tree 
--

treeToPreorder :: Tree Char -> String
treeToPreorder Empty = []
treeToPreorder (Branch a left right) = [a] ++ (treeToPreorder left) ++ (treeToPreorder right)

treeToInorder :: Tree Char -> String
treeToInorder Empty = []
treeToInorder (Branch a left right) = (treeToPreorder left) ++ [a] ++ (treeToPreorder right)

preInTree :: String -> String -> Maybe (Tree Char)
preInTree [] [] = Just Empty
preInTree [] _  = Nothing
preInTree _ []  = Nothing
preInTree po io = do 
    index <- elemIndex (head po) io
    let is = (take index io, drop (index+1) io)
    let _t = drop 1 po
    let ps = (take index _t, drop index _t)
    left <- preInTree (fst ps) (fst is)
    right <- preInTree (snd ps) (snd is)
    return (Branch (head po) left right)

{-
let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ; po = treeToPreorder t ; io = treeToInorder t } in preInTree po io >>= print
-}
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
    
