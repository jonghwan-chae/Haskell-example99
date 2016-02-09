--
-- Problem 50 :: (***) Huffman codes. 
{-
We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 

* Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. 
Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. 
In our example, the result could be  Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. 
The task shall be performed by the predicate huffman/2 defined as follows: 
-}

import Control.Monad
import Data.List

data Tree = Leaf { item :: (Char, Int), cnt :: Int } 
            | Tree { left :: Tree, right :: Tree, cnt :: Int } deriving Show 

ins :: Tree -> [Tree] -> [Tree]
ins i [] = [i]
ins i (a1:an) 
    | cnt i <= cnt a1  = i : a1 : an
    | otherwise       = a1 : ins i an

insTree :: [Tree] -> [Tree]
insTree [] = []
insTree (a1:an) = ins a1 (insTree an)

makeTree :: Tree -> Tree -> Tree
makeTree t1 t2 = Tree {left = t1, right = t2, cnt = (cnt t1) + (cnt t2)}

makeHuffman :: [Tree] -> [Tree]
makeHuffman lists
    | len == 1  = lists
    | otherwise = makeHuffman $ ins (makeTree (lists!!0) (lists!!1)) (drop 2 lists)
    where len = length lists

makeCode :: String -> Tree ->  [(Char, String)]
makeCode code htree = case htree of
    Leaf _ _    -> [(fst (item htree), code)]
    Tree _ _ _  -> (makeCode (code ++ "0") (left htree)) ++ (makeCode (code ++ "1") (right htree))


huffman :: [(Char, Int)] -> [(Char, String)]
huffman lists = makeCode [] $ head . makeHuffman $ insTree $ map (\(c, n) -> Leaf {item = (c, n), cnt = n} ) lists


{-
*Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
-}
