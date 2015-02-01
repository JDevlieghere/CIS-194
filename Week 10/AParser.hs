{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Functor instance for Parser
instance Functor Parser where
  fmap f p = Parser (\s ->
    case runParser p s of
      Nothing -> Nothing
      Just x  -> Just $ first f x)

-- Functor helper function
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

-- Applicative instance for Parser
instance Applicative Parser where
  pure x  = Parser (\s -> Just (x,s))
  f <*> x = Parser (\s ->
    case runParser f s of
      Nothing     -> Nothing
      Just (g,s') -> runParser (g <$> x) s')


-- Parser that expects to see the characters 'a' and 'b' and returns them as
-- a pair
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- Parser that expects to see the characters 'a' and 'b' and returns ()
abParser_ :: Parser ()
abParser_ = const () <$> abParser

-- Parser that reads two integer values separated by a space and returns the
-- integer values in a list
intPairParser :: Parser [Integer]
intPairParser = (\ i _ j -> [i,j]) <$> posInt <*> char ' ' <*> posInt

-- Alternative instance for Parser
instance Alternative Parser where
  empty   = Parser (const Nothing)
  f <|> g = Parser (\s ->
    case runParser f s of
      Nothing -> runParser g s
      Just x  -> Just x)

