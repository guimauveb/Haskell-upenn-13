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

-- Helper function: returns a list of tupples of items values and their occurence 
-- Example: occAndValue (countOcc [1,1,1,5]) (rmDups [1,1,1,5]) = [(3,1), (1,5)]
occAndValue :: [a] -> [b] -> [(a, b)]
occAndValue [] _ = []
occAndValue (x:xs) (y:ys) = (x,y) : occAndValue xs ys

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
orderByFirst :: Ord a => [(Int, a)] -> [(Int, a)]
orderByFirst xs = reverse $ sortBy (compare `on` fst) xs 

-- Merge two lists into one
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- First argument is [0..9]
printRow :: [Int] -> [Int] -> String
printRow [] _ = ""
printRow _ [] = ""
printRow (x:xs) ys
  | elem x ys == True = "*" ++ printRow xs ys
  | otherwise = " " ++ printRow xs ys

-- Fill missing values for yMax and x
-- Example: addMissingXs 2 6 -> [(2,6), (1,6)]
-- Allows to print a "*" for each row under x's yMax
addMissingXs :: Int -> Int -> [(Int, Int)]
addMissingXs yMax x = [(a,b) | a <- reverse $ [1..yMax], b <- [x]]

-- Fill missing values fo each (y,x)
addMissingXsAll :: [(Int,Int)] -> Int -> [(Int,Int)]
addMissingXsAll xs y 
  | (y==0) = [(0,0)]
  | otherwise = (concat $ fmap (addMissingXs y) (valuesAtY y xs)) ++ addMissingXsAll xs (y-1) 

-- histogram :: [Int] -> String
-- histogram [] = "Nothing to show."
-- histogram xs = 
--     concat $ (map (\y -> 
--       (printRow [0..9] $ valuesAtY y $ addMissingXsAll occurencesAndValues yMax) ++ "\n") $ reverse [1..yMax]) 
--       ++ ["==========\n0123456789\n"]
--     where occurencesAndValues = orderByFirst $ occAndValue (countOcc xs) (rmDups xs)
--           yMax = (histoHeight occurencesAndValues)

-------------------------------------------------------------------------------------------------------------------
-- Much better and elegant solution (from https://github.com/OctaviPascual/cis194-IntroductionToHaskell/tree/master/homework-03)
-- Count the occurence of a given number in the list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)


-- Generate as many stars as there are occurences of a given number and fill the rest with blanks
bar :: Int -> Int -> String
bar f maxF = take maxF $ replicate f '*' ++ repeat ' ' 

-- Now transform this horizontal notation to a vertical notation by rotating the bars 90° clockwise.
histogram :: [Integer] -> String
histogram xs = unlines $ rotate bars ++ legend
  where frequencies = [count i xs | i <- [0..9]]
        maxF = maximum frequencies
        bars = [bar f maxF | f <- frequencies]
        rotate = reverse . transpose
        legend = ["==========", "0123456789"]




