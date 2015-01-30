xor :: [Bool] -> Bool
xor = foldl (\x y -> (x || y) && not (x && y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x y -> f x : y) []