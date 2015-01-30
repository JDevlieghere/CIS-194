sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ filter (`notElem` crossed) [1..n]
    where crossed = [ i+j+2*i*j | i <- [1..n], j <- [1..n], i <= j, i+j+2*i*j <= n]