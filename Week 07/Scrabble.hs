module Scrabble where

import Data.Char
import Data.Monoid

data Score = Score Int deriving (Show, Eq)

instance Num Score where
  (Score a) + (Score b) = Score (a+b)
  (Score a) * (Score b) = Score (a*b)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c
    | lc `elem` "aeilnorstu" = Score 1
    | lc `elem` "dg"         = Score 2
    | lc `elem` "bcmp"       = Score 3
    | lc `elem` "fhvwy"      = Score 4
    | lc `elem` "k"          = Score 5
    | lc `elem` "jx"         = Score 8
    | lc `elem` "qz"         = Score 10
    | otherwise              = Score 0
    where lc = toLower c

scoreVal :: Score -> Int
scoreVal (Score x) = x

scoreString :: String -> Score
scoreString = foldl (+) (Score 0) . map score
