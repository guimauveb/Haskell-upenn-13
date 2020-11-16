module Golf where 

-- Exercice 1
-- Returns every nth element of a list
everyN :: Int -> [a] -> [a]
everyN n xs = case drop (n-1) xs of
               (y:ys) -> y : everyN n ys
               [] -> []

-- The first list in the output should be the same as the input list
-- The nth list in the ouput should contain every nth element from the input list
skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
-- We can call everyN for n=2, n=3 ... but how to get n ?
-- Answer: By using a lambda function: calling n for n between 1 and the length of the list
skips xs = map (\n -> everyN n xs) [1..length xs]
-- Can be written as:
-- skips xs = (\n -> everyN n xs) <$> [1..length xs]

-- Exercice 2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs) 
  | (max x (max y z) == y) = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
                     

main = undefined
