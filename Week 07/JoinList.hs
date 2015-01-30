{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

-- Join List

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)

-- Yields a new JoinList whose monoidal annotation is derived from those of the
-- two arguments
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
left +++ right = Append newTag left right
    where newTag = mappend (tag left) (tag right)

-- Get the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _ ) = m

tSize :: (Sized b, Monoid b) => JoinList b a -> Int
tSize = getSize . size . tag

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Finds the JoinList element at the specified index.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                  = Nothing
indexJ _ (Single _ a)           = Just a
indexJ i jl@(Append _ left right)
    | i < 0         = Nothing
    | i > thisSize  = Nothing
    | i < leftSize  = indexJ i left
    | otherwise     = indexJ (i - leftSize) right
    where leftSize = tSize left
          thisSize = tSize jl

-- Drop the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _) = if n > 0 then Empty else jl
dropJ n jl@(Append _ left right)
    | n < 0        = jl
    | n > thisSize = Empty
    | n < leftSize = dropJ n left +++ right
    | otherwise    = dropJ (n - leftSize) right
    where leftSize = tSize left
          thisSize = tSize jl

-- Return the first n elements of a JoinList, dropping all other elements
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ _) = if n > 0 then jl else Empty
takeJ n jl@(Append _ left right)
    | n < 0         = Empty
    | n >= thisSize = jl
    | n < leftSize  = takeJ n left
    | otherwise     = left +++ takeJ (n - leftSize) right
    where leftSize  = tSize left
          thisSize  = tSize jl

-- Scrabble Scoring

type JLBuffer = JoinList (Score, Size) String

-- Return a new Join List with the score of the line
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Return a new Join List Buffer with the score of the line
bufferScoreLine :: String -> JLBuffer
bufferScoreLine s = Single (scoreString s, Size 1) s

instance Buffer JLBuffer where
    -- toString :: b -> String
    toString          = unlines . jlToList
    -- fromString :: String -> b
    fromString        = foldl (+++) Empty . map bufferScoreLine . lines
    -- line :: Int -> b -> Maybe String
    line              = indexJ
    -- replaceLine :: Int -> String -> b -> b
    replaceLine n s b = takeJ n b +++ fromString s +++ dropJ (n+1) b
    -- numLines :: b -> Int
    numLines          = tSize
    -- value :: b -> Int
    value             = scoreVal . fst . tag

