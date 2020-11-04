-- CIS 194 Homework 2

{- OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


-- 1. Break the string into words using ' '
splitLog :: Char -> String -> [String]
splitLog c xs =
  -- break returns a tuple
    case break (c==) xs of
      -- If right side == empty string
      (ls, "") -> [ls]
      -- Else use recursion
      (ls, x:rs) -> ls : splitLog c rs

-- 2 Check message type via the first item of the list
detMessageType :: [String] -> MessageType
detMessageType (x:xs)
  | (x=="E") = Error num 
  | (x=="I") = Info
  | (x=="W") = Warning
-- Added Incorrect to MessageType to catch format error
  | otherwise = Incorrect
  where num = read (xs !! 0) :: Int

-- 3. Determine the timestamp. We basically cast the string into an Int (TimeStamp)
detTimeStamp :: String -> TimeStamp
detTimeStamp x = read x :: Int

-- 4. Convert the rest of the tokens back to a string
-- unwords will join words with a space char. For a more general solution we can use intercalate / intersperse from
-- Data.List
detMsg :: [String] -> String
detMsg x = unwords x

parseMessage :: String -> LogMessage
parseMessage "" = Unknown "Empty string"
parseMessage x = 
  -- Check if the first char is actually a MessageType ('E' || 'W' || 'I')
      let tokens = splitLog ' ' x
       in case detMessageType tokens of 
            -- Create a function for each message type ?
        Warning -> LogMessage Warning ts msg 
          where ts = detTimeStamp (tokens !! 1)
                -- We drop the 2 first items corresponding to the message type and the timestamp
                msg = detMsg $ drop 2 $ tokens  
        Info -> LogMessage Info ts msg
          where ts = detTimeStamp (tokens !! 1)
                -- We drop the 2 first items corresponding to the message type and the timestamp
                msg = detMsg $ drop 2 $ tokens  
        -- Error Int !
        Error n -> LogMessage err ts msg
          where err = detMessageType tokens
                ts = detTimeStamp (tokens !! 2)
                -- We drop the 3 first items corresponding to the message type, the error number 
                -- and the timestamp
                msg = detMsg $ drop 3 $ tokens
        Incorrect -> Unknown "This is not in the right format."

splitInput :: [String] -> [LogMessage]
splitInput [] = [Unknown "Empty list"]
splitInput (x:[]) = [parseMessage x]
splitInput (x:xs) = parseMessage x : splitInput xs 

-- See how I can elegantly mix splitInput and parse
parse :: String -> [LogMessage]
parse s = splitInput $ splitLog '\n' s  

{- 

BinaryTree example:

      4
     / \
    2   5
   / \
  1   3

 MessageTree example with timestamps corresponding to the previous binary tree values:

                                                              LogMessage (Error 4) 444 "Top error message"
                                                                    /                             \ 
                        LogMessage (Error 2) 222  "Middle left error message"                 LogMessage (Error 5) 555 "Middle right error message"   
                                                  /         \ 
LogMessage (Error 1) 111  "Bottom left error message"     LogMessage (Error 3) 333 "Bottom right error message"

-}

-- MessageTree example width multiple nodes - corresponds to the tree written above
msgTree = Node (Node (Node (Leaf) (LogMessage (Error 1) 111 "Bottom left message") (Leaf) ) (LogMessage (Error 2) 222 "Middle left message") (Node (Leaf) (LogMessage (Error 3) 333 "Bottom right message") (Leaf))) (LogMessage (Error 4) 444 "Top message") (Node (Leaf) (LogMessage (Error 5) 555 "Middle right message") (Leaf))

-- Order by (logMsg, right, left)
valuesPreOrder :: MessageTree -> [LogMessage]
valuesPreOrder Leaf = []
valuesPreOrder (Node (Leaf) (logMsg) (Leaf))  = [logMsg]
valuesPreOrder (Node (leftSubTree) (logMsg) (rightSubTree)) = [logMsg] ++ valuesPreOrder leftSubTree ++ valuesPreOrder rightSubTree 

preOrder = [1,3,5,4,2]


-- Order by (left, logMsg, right)
valuesInOrder :: MessageTree -> [LogMessage]
valuesInOrder Leaf = []
valuesInOrder (Node (Leaf) (logMsg) (Leaf))  = [logMsg]
valuesInOrder (Node (leftSubTree) (logMsg) (rightSubTree)) = valuesInOrder leftSubTree ++ [logMsg] ++ valuesInOrder rightSubTree

inOrder = [5,3,4,1,2]


-- Order by (left, right, logMsg)
valuesPostOrder :: MessageTree -> [LogMessage]
valuesPostOrder Leaf = []
valuesPostOrder (Node (Leaf) (logMsg) (Leaf))  = [logMsg]
valuesPostOrder (Node (leftSubTree) (logMsg) (rightSubTree)) = valuesPostOrder leftSubTree ++ valuesPostOrder rightSubTree ++ [logMsg] 

postOrder = [5,4,3,2,1]


-- Find the minimum LogMessage timestamp in a tree of LogMessages and return the LogMessage
minMsgTree :: MessageTree -> LogMessage
minMsgTree Leaf = (Unknown "Empty message")
minMsgTree (Node (leftSubTree) (logMsg) (rightSubTree)) = minLogMsg logMsg (minLogMsg leftLogMsg rightLogMsg) 
  where 
    leftLogMsg = if (leftSubTree == Leaf) then logMsg
                                          else minMsgTree (leftSubTree)
    rightLogMsg = if (rightSubTree == Leaf) then logMsg
                                            else minMsgTree (rightSubTree)


-- Find the maximum LogMessage timestamp in a tree of LogMessages and return the LogMessage 
maxMsgTree :: MessageTree -> LogMessage
maxMsgTree Leaf = (Unknown "Empty message")
maxMsgTree (Node (leftSubTree) (logMsg) (rightSubTree)) = maxLogMsg logMsg (maxLogMsg leftLogMsg rightLogMsg) 
  where 
    leftLogMsg = if (leftSubTree == Leaf) 
                    then logMsg
                 else maxMsgTree (leftSubTree)
    rightLogMsg = if (rightSubTree == Leaf)
                     then logMsg
                  else maxMsgTree (rightSubTree)


-- Return the log that has the lowest timestamp
minLogMsg :: LogMessage -> LogMessage -> LogMessage
minLogMsg (Unknown _) _ = Unknown "Empty message"
minLogMsg a b = 
  if (x < y) then a else b
    where 
      x = getTimeStamp(a)
      y = getTimeStamp(b)


-- Return the log that has the highest timestamp
maxLogMsg :: LogMessage -> LogMessage -> LogMessage
maxLogMsg (Unknown _) _ = Unknown "Empty message"
maxLogMsg a b = 
  if (x > y) then a else b
    where 
      x = getTimeStamp(a)
      y = getTimeStamp(b)


-- Check if a binary tree is properly sorted (if it is actually a search tree) 
isMsgSTree :: MessageTree -> Bool
isMsgSTree Leaf = True
isMsgSTree (Node (leftSubTree) (logMsg) (rightSubTree)) = isMsgSTree rightSubTree &&
  isMsgSTree leftSubTree &&
    (leftSubTree  == Leaf || logMsg > maxMsgTree(leftSubTree)) &&
    (rightSubTree == Leaf || logMsg < minMsgTree(rightSubTree)) 


searchMsgTree :: LogMessage -> MessageTree -> Bool
searchMsgTree  _ Leaf = False
searchMsgTree (Unknown _) _ = False
searchMsgTree a (Node (leftSubTree) (logMsg) (rightSubTree))
  | (a == logMsg) = True
  | (a > logMsg) = searchMsgTree  (a) (rightSubTree)
  | otherwise = searchMsgTree (a) (leftSubTree)


-- Insert a new LogMessage into an existing MessageTree, producing a new MessageTree
-- Search for the LogMessage in the tree
  -- if it's found -> return the tree untouched
  -- else add it to the correct position. This way the resulting tree should always be sorted

insert :: LogMessage -> MessageTree -> MessageTree
insert newLog Leaf = Node Leaf newLog Leaf 
insert (Unknown _) msgTree = msgTree
insert newLog (Node left logMsg right) 
  -- if the newLog is already in the tree, return the tree untouched
    | (newLog == logMsg)  = Node left logMsg right
    | (newLog < logMsg)   = Node (insert (newLog) left) logMsg right
    | otherwise           = Node left logMsg (insert (newLog) right)


build :: [LogMessage] -> MessageTree
build [] = Leaf
build logMsgs = beforeInsert logMsgs Leaf


-- helper function needed since we need to pass the growing tree to the function
-- (there probably is a better way to do this)
beforeInsert :: [LogMessage] -> MessageTree -> MessageTree
beforeInsert [] msgTree = msgTree
beforeInsert (l:[]) msgTree = insert l msgTree
beforeInsert (l:ls) msgTree = beforeInsert (ls) (t)
  where t = insert (l) (msgTree)

logMsgs = parse ("I 5053 pci_id: con ing!\nI 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)\nW 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled\nI 4076 verse.'\nI 4764 He trusts to you to set them free\nI 858 your pocket?' he went on, turning to Alice.\nI 898 would be offended again.\nI 3753 pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)\nI 790 those long words, and, what's more, I don't believe you do either!' And\nI 3899 hastily.\nI 2194 little creature, and held out its arms and legs in all directions, 'just\nI 1447 she was terribly frightened all the time at the thought that it might be")




