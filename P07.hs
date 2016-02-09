--
-- Problem 07

data NestedList a = Elem a | List [NestedList a]

flatten :: Num a => NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap (flatten) x


main :: IO()
main = do
    putStrLn $ show $ flatten (Elem 5)
    putStrLn $ show $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    putStrLn $ show $ flatten (List [])

