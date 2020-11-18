module Golf where 

import Data.List

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
-- Example: oneList ["Hello", "world", "!"] [0, 1, 2] = [("Hello", 0), ("world", 1), ("!", 2)]
oneList :: [a] -> [b] -> [(a, b)]
oneList [] _ = []
oneList (x:xs) (y:ys) = (x,y) : oneList xs ys

itemAndOcc :: (Ord a) => [a] -> [a] -> [(a, Int)]
itemAndOcc xs = undefined

histogram :: [Integer] -> String
histogram [] = ""
histogram (x:xs)
  -- Sort the list DONE
  -- Find the most occuring item -> it will give us the height of the histogram DONE
  -- Draft: (1, 4times), (2, 4times), (3, 2times) 
  -- putStrLn " **\n"
  -- putStrLn " **\n"
  -- putStrLn " **\n"
  -- putStrln " ***\n"
  -- putStrLn "=========\n"
  -- putStrLn "0123456789"
  | otherwise = ""



main = undefined
