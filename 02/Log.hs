-- CIS 194 Homework 2

module Log where
import Control.Applicative 

data MessageType = Info 
                 | Warning 
                 | Error Int 
                 | Incorrect 
                 deriving (Show, Eq) 

type TimeStamp = Int 

data LogMessage = LogMessage MessageType TimeStamp String 
                | Unknown String 
                deriving (Show, Eq) 

-- Deriving Ord so we can easily compare log messages by TimeStamp
instance Ord LogMessage where
  (LogMessage _ ts _) `compare` (LogMessage _ ts2 _) = ts `compare` ts2


-- Creating getters instead of modifying the original type and use records
getMessageType :: LogMessage -> MessageType
getMessageType (LogMessage msgType _ _) = msgType

getTimeStamp :: LogMessage -> Int
getTimeStamp (LogMessage _ timeStamp _) = timeStamp

getString :: LogMessage -> String
getString (LogMessage _ _ str) = str


data MessageTree = Leaf 
                 | Node MessageTree LogMessage MessageTree 
                 deriving (Show, Eq)


-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file = whatWentWrong . parse <$> readFile file

