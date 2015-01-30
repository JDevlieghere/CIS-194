{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Control.Monad

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

sortedThrow :: Int -> Rand StdGen [DieValue]
sortedThrow x = do
    t1 <- die
    t2 <- die
    t3 <- die
    let throws = take x [t1,t2,t3]
    return $ sortBy (flip compare) throws

count :: ((a,b)-> Bool) -> [(a,b)] -> Int
count f = length . filter f


battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    attackersThrow <- sortedThrow (attackers bf - 1)
    defendersThrow <- sortedThrow (defenders bf)
    let zipped = zip attackersThrow defendersThrow
        newAttackers = attacking - length zipped + count (uncurry (>)) zipped
        newDefenders = defending - length zipped + count (uncurry (<)) zipped
    return bf { attackers = newAttackers, defenders = newDefenders }
    where
        attacking = attackers bf
        defending = defenders bf
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
    outcome <- battle bf
    if defeated outcome
        then return outcome
        else invade outcome

defeated :: Battlefield -> Bool
defeated bf = defenders bf <= 2

destroyed :: Battlefield -> Bool
destroyed bf = defenders bf == 0

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    bfs <- replicateM 1000 (invade bf)
    let destroyedCount = length (filter destroyed bfs)
    return $ ((/1000) . fromIntegral) destroyedCount

main :: IO()
main = do
  ratio <- evalRandIO $ successProb bf
  print ratio
  where
    bf = Battlefield {attackers = 3, defenders = 2}