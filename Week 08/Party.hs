{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

instance Ord Employee where
  compare l r = compare (empName l) (empName r)

glEmpty :: GuestList
glEmpty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons e (GL xs fun) = GL (e:xs) (fun + empFun e)

glSort :: GuestList -> GuestList
glSort (GL xs f) = GL (sort xs) f

instance Monoid GuestList where
    mempty                        = GL [] 0
    mappend (GL ls lf) (GL rs rf) = GL (ls ++ rs) (lf + rf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun x y = if x > y then x else y

treeFold ::  (a -> b -> b) -> b -> Tree a -> b
treeFold f b = foldr f b . flatten

-- Function that returns the best possible guest list we can create if we invite
-- the boss and the the best possible guest list we can create if we don't
-- invite the boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss best = (bestWithCurrentBoss, bestWithoutCurrentBoss)
    where (withSubBoss, withoutSubBoss) = unzip best
          bestWithSubBoss               = maximum withSubBoss
          bestWithoutSubBoss            = maximum withoutSubBoss
          bestWithCurrentBoss           = maximum [glCons boss bestWithSubBoss, glCons boss bestWithoutSubBoss]
          bestWithoutCurrentBoss        = maximum [bestWithSubBoss, bestWithoutSubBoss]

-- Takes a company hierarchy as input and outputs a fun-maximizing guest list.
maxFun :: Tree Employee -> GuestList
maxFun = undefined

-- Reads the company's hierarchy from the file company.txt, and then print out
-- a formatted guest list, sorted by first name
main :: IO ()
main = do
    contents <- readFile "company.txt"
    let company = read contents :: Tree Employee
    print $ maxFun company