import System.IO

type Peg = String

type Move = (Peg, Peg)

-- x = number of discs a =, start b = end, c = temp
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
-- hanoi will output a Move when called with x = 1
hanoi 1 start _ end = [(start, end)]
hanoi x start temp end =
  let xMinusOne = subtract 1 x
   in hanoi xMinusOne start end temp ++
     hanoi 1 start temp end ++ 
       hanoi xMinusOne temp start end

main = print (hanoi 4 "a" "b" "c")
