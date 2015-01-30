data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Convert a Stream to an infinite list
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Generates a stream containing infinitely many copies of the given element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- Generate a Stream from a "seed" of type a, which is the  first element of the
-- stream, and an "unfolding rule" of type a -> a which specifies how to
-- transform the seed into a new seed, to be used for generating the rest of the
-- stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- The infinite list of natural numbers 0, 1, 2, ...
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Alternates the elements from two streams
interleaveStreams :: Stream a -> Stream a -> Bool -> Stream a
interleaveStreams l@(Cons lx lxs) r@(Cons rx rxs) first = if first
        then Cons lx (interleaveStreams lxs r opposite)
        else Cons rx (interleaveStreams l rxs opposite)
    where opposite = not first

rulerNaive :: Stream Integer
rulerNaive = streamMap (\n -> maximum (0:[ p | p <- [1..n],  (n `rem` 2^p) == 0])) posNats
    where posNats = streamFromSeed (+1) 1

