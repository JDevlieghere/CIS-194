{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Maybe
import System.Environment

-- Parsing

parseWord :: String -> Maybe (String, String)
parseWord [] = Nothing
parseWord s  = Just (x, unwords xs)
    where (x:xs) = words s

parseType :: String -> Maybe(MessageType, String)
parseType s = do
    (typeName,r) <- parseWord s
    toType typeName r
    where
        toType "I" r = Just (Info, r)
        toType "W" r = Just (Warning, r)
        toType "E" r = do
            (value, r2) <- parseWord r
            Just (Error (read value::Int) , r2)
        toType _ _ = Nothing

parseTimeStamp :: String -> Maybe(TimeStamp, String)
parseTimeStamp s = do
    (timeStamp,r) <- parseWord s
    return (read timeStamp :: Int, r)

parseAll :: String -> Maybe LogMessage
parseAll s = do
    (messageType,r) <- parseType s
    (timeStamp, r2)  <- parseTimeStamp r
    return $ LogMessage messageType timeStamp r2


parseMessage :: String -> LogMessage
parseMessage s = fromMaybe (Unknown s) (parseAll s)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Naive Sorting

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ time _) = time

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left node right) = if earlier
                                    then insert msg left
                                    else insert msg right
    where earlier = getTimeStamp msg < getTimeStamp node

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left node right) = inOrder left ++ [node] ++ inOrder right

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = inOrder . build

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map show . filter isRelevant
    where isRelevant (LogMessage messageType _ _) = case messageType of
            Error level -> level > 50
            _ -> False
          isRelevant (Unknown _) = False

-- Main

main :: IO ()
main = do
    (args:_) <- getArgs
    content <- readFile args
    putStrLn $ unlines (whatWentWrong (parse content))