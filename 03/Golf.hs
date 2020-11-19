module Golf where 

import Data.Function(on)
import Prelude
import Data.List
import Data.Ord
import Control.Lens

-- Exercice 1
-- Returns every nth element of a list
everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n-1) xs of
               (y:ys) -> y : everyNth n ys
               [] -> []

-- The first list in the output should be the same as the input list
-- The nth list in the ouput should contain every nth element from the input list
skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
-- We can call everyN for n=2, n=3 ... but how to get n ?
-- Answer: By using a lambda function: calling n for n between 1 and the length of the list
skips xs = map (\n -> everyNth n xs) [1..length xs]
-- Can be written as:
-- skips xs = (\n -> everyN n xs) <$> [1..length xs]

-- Exercice 2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs) 
  | (max x (max y z) == y) = y : tail
  | otherwise = tail
  where tail = localMaxima (y:z:xs)
                     
-- Exercice 3

-- Helper function: removes duplicates from a list
rmDups :: (Ord a) => [a] -> [a]
rmDups = map head . group . sort

-- Helper function: returns a list of item occurences in a sorted list
countOcc :: (Ord a) => [a] -> [Int]
countOcc = map length . group . sort 

-- Helper function: returns a tupple of items sharing indexes in two lists
-- Example: oneList (countOcc [1,1,1,5]) (rmDups [1,1,1,5]) = [(3,1), (1,5)]
occAndItem :: [a] -> [b] -> [(a, b)]
occAndItem [] _ = []
occAndItem (x:xs) (y:ys) = (x,y) : occAndItem xs ys

histoHeight :: [(Int, a)] -> Int
histoHeight [] = 0
histoHeight ((n, v):xs) = n

-- Values at a given row
valuesAtY :: Int -> [(Int, a)] -> [a]
valuesAtY _ [] = []
valuesAtY x ((n, v):xs) 
  | (x==n) = v : valuesAtY x xs
  | otherwise  = valuesAtY x xs

-- NOTE: try using sortBy and comparing
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- comparing :: (Ord b) => (a -> b) -> a -> a -> Ordering
orderByFst :: Ord a => [(Int, a)] -> [(Int, a)]
orderByFst xs = reverse $ sortBy (compare `on` fst) xs 

orderBySnd :: Ord a => [(Int, a)] -> [(Int, a)]
orderBySnd xs = reverse $ sortBy (compare `on` snd) xs 

-- If elem 0 [Int] -> "*"
-- If elem 1 [Int] -> ++ "*"
-- skips xs = map (\n -> everyNth n xs) [1..length xs]
printRow :: [Int] -> [Int] -> String
printRow [] _ = ""
printRow _ [] = ""
printRow (x:xs) ys
  | elem x ys == True = "*" ++ printRow xs ys
  | otherwise = " " ++ printRow xs ys

histogram :: [Integer] -> String
histogram [] = ""
histogram xs
  -- Sort the list DONE
  -- Find the most occuring item -> it will give us the height of the histogram DONE
  -- Draft: Make a list of tupples (a,b) where a = occurences and b = value DONE
  -- tmp = occAndItem (countOcc xs) (rmDups xs)
  -- vals = valuesAtY n $ orderByFst tmp 
  -- if vals != null -> putStrLn "*" val1 putStrLn valn ...
  -- putStrLn "=========\n"
  -- putStrLn "0123456789"
  | otherwise = ""


main = undefined
