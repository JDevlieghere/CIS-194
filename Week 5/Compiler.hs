{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import StackVM
import Parser

-- Stack VM

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr Program where
    lit x = [PushI x]
    mul x y = x ++ y ++ [Mul]
    add x y = x ++ y ++ [Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul

evalStr :: String -> Either String StackVal
evalStr s = case compiled of
        Nothing   -> Left "Compilation failed"
        Just prog -> stackVM prog
    where compiled = compile s

-- Main
main :: IO()
main = undefined