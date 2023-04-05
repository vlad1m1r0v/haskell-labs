import Data.List (intercalate, transpose, nub)
--task 1
--with prelude (elem / nub is function defined in Prelude library)
removeDuplicatesPrelude :: Eq a => [a] -> [a]
-- removeDuplicatesPrelude = nub
removeDuplicatesPrelude [] = []
removeDuplicatesPrelude (x:xs)
    | x `elem` xs = removeDuplicatesPrelude xs
    | otherwise   = x : removeDuplicatesPrelude xs

--without prelude
--custom elem implementation
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
    | x == y    = True
    | otherwise = myElem x ys

removeDublicatesWithoutPrelude :: Eq a => [a] -> [a]
removeDublicatesWithoutPrelude [] = []
removeDublicatesWithoutPrelude (x:xs)
    | myElem x xs = removeDublicatesWithoutPrelude xs
    | otherwise   = x : removeDublicatesWithoutPrelude xs

--task 2
--with prelude (take and min are functions defined in Prelude)
mergePrelude :: [a] -> [a] -> [a]
mergePrelude [] _ = []
mergePrelude _ [] = []
mergePrelude xs ys = take (2 * n) (intercalate [] (transpose [xs, ys])) 
    where n = min (length xs) (length ys)
--without prelude
mergeWithoutPrelude :: [a] -> [a] -> [a]
mergeWithoutPrelude [] _ = []
mergeWithoutPrelude _ [] = []
mergeWithoutPrelude (x:xs) (y:ys) = x:y:mergeWithoutPrelude xs ys


main = do
    putStrLn "TASK 1.A: WITH PRELUDE"
    print p1
    putStrLn "TASK 1.B: WITHOUT PRELUDE"
    print wp1
    putStrLn "TASK 2.A: WITH PRELUDE"
    print p2
    putStrLn "TASK 2.B: WITHOUT PRELUDE"
    print wp2
    where
        p1 = removeDuplicatesPrelude arr0
        wp1 = removeDublicatesWithoutPrelude arr0
        p2 = mergePrelude arr1 arr2
        wp2 = mergeWithoutPrelude arr1 arr2 
        arr0 = [1,1,1,5,5,3, 1,1,222,222,222,222]
        arr1 = "abcde"
        arr2 = "123"