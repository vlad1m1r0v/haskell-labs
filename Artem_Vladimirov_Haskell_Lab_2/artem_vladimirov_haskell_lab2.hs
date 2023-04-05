import Data.List (sort, group, delete)
import qualified Data.Map.Strict as Map

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myMod :: Int -> Int -> Int
myMod x y
  | x < y     = x
  | otherwise = myMod (x - y) y

myAll :: (a -> Bool) -> [a] -> Bool
myAll p [] = True
myAll p (x:xs)
  | p x = myAll p xs
  | otherwise = False

--task 1
--with prelude (foldl is function defined in Prelude module)
frequencyPrelude :: Ord a => [a] -> [(a, Int)]
frequencyPrelude xs = Map.toList $ foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty xs

--without prelude
frequencyWithoutPrelude :: Ord a => [a] -> [(a, Int)]
frequencyWithoutPrelude xs = Map.toList $ myFoldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty xs

--task 2
--with prelude (mod, all, floor, sqrt, fromIntegral are functions defined in Prelude module)
isPrimePrelude :: Int -> Bool
isPrimePrelude n
  | n <= 1 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..limit]
  where limit = floor $ sqrt $ fromIntegral n
--without prelude
isPrimeWithoutPrelude :: Int -> Bool
isPrimeWithoutPrelude n
  | n <= 1 = False
  | otherwise = myAll (\x ->  myMod n x /= 0) [2..n-1]

main = do
    putStrLn "TASK 1.A: WITH PRELUDE"
    let strP1 = map ((++ "\n") . show) p1
    putStrLn $ concat strP1
    putStrLn "TASK 1.B: WITHOUT PRELUDE"
    let strWp1 = map ((++ "\n") . show) wp1
    putStrLn $ concat strWp1
    putStrLn "TASK 2.A: WITH PRELUDE"
    let strP2 = map ((++ "\n") . show) p2
    putStrLn $ concat strP2
    putStrLn "TASK 2.B: WITHOUT PRELUDE"
    let strWp2 = map ((++ "\n") . show) wp2
    putStrLn $ concat strWp2
    where
        p1 = map frequencyPrelude arr1
        wp1 = map frequencyWithoutPrelude arr1
        p2 = map isPrimePrelude arr2
        wp2 = map isPrimeWithoutPrelude arr2
        arr1 = ["aaabbcaadddd","22222133333ddd","00022213888833DDDFF"]
        arr2 = [30,31,32,100,101]
