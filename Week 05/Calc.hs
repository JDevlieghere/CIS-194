import ExprT
import Parser
import Data.Maybe

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

evalStr :: String -> Maybe Integer
evalStr s = case parsed of
        Nothing  -> Nothing
        Just exp -> Just (eval exp)
    where parsed = parseExp Lit Add Mul s

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Main
main :: IO()
main = undefined