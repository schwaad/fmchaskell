module List where
    import Prelude
        hiding (replicate, map, filter)
    import Nat

    replicate :: Nat -> a -> [a]
    replicate O _ = []
    replicate (S n) x = x : (replicate n x)

    map :: (a -> b) -> [a] -> [b]
    map f [] = []
    map f (x:xs) = (f x) : (map f xs)

    filter :: (a -> Bool) -> [a] -> [a]
    filter f [] = []
    filter f (x:xs)
        | f x = x : filter f xs
        | otherwise = filter f xs

    