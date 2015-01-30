import Data.List

skips :: [a] -> [[a]]
skips = init . tails