import System.IO

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | (x == 0) = []
  | (x < 0) = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

-- Can we do it without using a helper function ?
toDigits :: Integer -> [Integer]
toDigits x
  | (x == 0) = []
  | (x < 0) = []
  | otherwise = reverseList (toDigitsRev x)

-- toDigitsRev must actually receive an Integer and return a reversed list
reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
-- More explicit for me than another possible solution: 
-- doubleEveryOther = zipWith ($) (cycle [id,(*2)]
-- starting from the right
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse

-- tokenize a digit and returns a list of its tokens
tokenizeDigit :: Integer -> [Integer]
tokenizeDigit = map (`mod` 10) . takeWhile (/= 0) . iterate (`div` 10)

-- calls tokenizeDigit for all digits in a list
tokenizeListOfDigits :: [Integer] -> [Integer]
tokenizeListOfDigits [] = []
tokenizeListOfDigits (x:xs) = tokenizeDigit x ++ tokenizeListOfDigits xs

-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits x = sum $ tokenizeListOfDigits x

validate :: Integer -> Bool
validate x
  | mod (sumDigits $ doubleEveryOther $ toDigits x) 10 == 0 = True
  | otherwise = False

main = do
  putStrLn "Enter a credit card number:"
  line <- getLine
  if validate (read line)
    then putStrLn "CC number is valid."
    else putStrLn "CC number is invalid."
