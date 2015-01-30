localMaxima :: [Integer] -> [Integer]
localMaxima xs = reverse $ f xs []
    where f (a:b:c:xs) x = let nx = if b > a && b > c
                                    then b:x
                                    else x
                           in f (b:c:xs) nx
          f _ x = x