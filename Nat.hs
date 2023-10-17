module Nat where
  import Prelude
    hiding ((+), (*), (^), (-), (<), (>), (++), quot, min, gcd, lcm, div, max, pred, rem,
    length, elem, sum, product, reverse, enumFromTo, enumTo, take, drop, init, last, minimum, maximum, isPrefixOf, map)

data Nat = O | S Nat
    deriving ( Eq , Show )

(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)

(*) :: Nat -> Nat -> Nat
m * O = O
m * (S n) = m + (m * n)

fact :: Nat -> Nat
fact O = (S O)
fact (S m) = (S m) * fact m

fib :: Nat -> Nat
fib O = O
fib (S O) = (S O)
fib (S n) =  fib n + (S n)