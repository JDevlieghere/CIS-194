data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show,Eq)


foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

height :: Tree a -> Integer
height Leaf         = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf   = Node 0 Leaf x Leaf
insert x (Node h Leaf r Leaf)  = Node (h+1) (insert x Leaf) r Leaf
insert x (Node h Leaf r right) = Node h (insert x Leaf) r right
insert x (Node h left r Leaf)  = Node h left r (insert x Leaf)
insert x (Node h left r right)
        | height left > height right = Node newHeight left r (insert x right)
        | otherwise                  = Node newHeight (insert x left) r right
    where newHeight = 1 + max (height left) (height right)