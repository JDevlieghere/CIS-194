module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Takes a parser as input and runs it consecutively as many times as possible
-- (which could be none, if it fails right away), returning a list of the
-- results.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

-- Similar to zeroOrMore, except that it requires the input parser to succeed
-- at least once.
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p
                  <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- Parser which parses a consecutive list of zero or more whitespace characters
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- Parser which parses an identifier, which for our purposes will be an
-- alphabetic character followed by zero or more alphanumeric characters
ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha)
             <*> zeroOrMore (satisfy isAlphaNum)

-- Returns a new parser that strips all leading and trailing white space
stripped :: Parser a -> Parser a
stripped f = spaces *> f <* spaces

-- Returns a new parser with opening and closing brackets stripped, including
-- the white space around them
enclosed :: Parser a -> Parser a
enclosed f = stripped (char '(' *> stripped f <* char ')')

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only those Strings
-- consisting of a letter followed by any number of letters and digits are
-- valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr =  enclosed (Comb <$> zeroOrMore (stripped parseSExpr))
          <|> (A . N <$> stripped posInt)
          <|> (A . I <$> stripped ident)