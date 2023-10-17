module List where
    import Prelude
    import Nat

replicate :: Nat -> a -> [a]
replicate 0 _ = []
replicate (S n) _ = 