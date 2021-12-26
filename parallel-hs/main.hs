import Data.Int

factorials :: Int64 -> Int64 -> Int64 -> (Int64,Int64,Int64) -> Int64 -> Int64 -> (Int64,Int64,Int64)
factorials n k p (x,y,z) i f =
  if i > n
    then (x,y,z)
    else factorials n k p (x',y',z') i' f'
  where
    x' = if i == n then f else x
    y' = if i == (n-k) then f else y
    z' = if i == k then f else z
    i' = i+1
    f' = (f * i') `mod` p

exponentiate :: Int64 -> Int64 -> Int64 -> Int64
-- Calculates (a ^ n) mod p
exponentiate _ 0 _ = 1

exponentiate a n p =
  if n `mod` 2 == 0
    then half_n_squared
    else (half_n_squared * a) `mod` p
  where
    half_n_squared = (half_n ^ 2) `mod` p
    half_n = exponentiate a (n `div` 2) p

inverse :: Int64 -> Int64 -> Int64
-- Calculates the inverse of x in modulo p
inverse x p = exponentiate x (p-2) p


solve n k p = binom
  where
    (n_fact, n_k_fact, k_fact) = factorials n k p (1,1,1) 0 1
    n_k_inv = inverse n_k_fact p
    k_inv = inverse k_fact p
    binom = (((n_fact * n_k_inv) `mod` p) * k_inv) `mod` p

main = do
  return ()
