module HigherOrder where
import Prelude

-- Exercice 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) 
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)
-- fun1' = product . map (subtract 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n     = n + fun2 (n `div` 2)
       | otherwise  = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum 
      . filter even     
      . takeWhile (/= 1)
      . iterate (\n -> if even n then (n `div` 2) else (3 * n + 1)) 

-- Exercice 2: Folding with trees
-- Each node stores an extra Integer representing the height at that node
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)


