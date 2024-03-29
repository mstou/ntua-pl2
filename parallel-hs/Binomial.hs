module Binomial(inverse, nom, den) where
  import Data.Int
  
  exponentiate :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
  -- Calculates (a ^ n) mod p
  exponentiate _ 0 _ result = result

  exponentiate a n p r = exponentiate a' n' p r'
    where
      a' = (a*a) `mod` p
      n' = n `div` 2
      r' = if n `mod` 2 == 1 then (r*a) `mod` p else r

  inverse :: Int64 -> Int64 -> Int64
  -- Calculates the inverse of x in modulo p
  inverse x p = exponentiate x (p-2) p 1

  nom :: Int64 -> Int64 -> Int64 -> Int64
  nom n k p = foldl (\x acc -> (x*acc) `mod` p) 1 [n-k+1..n]

  den :: Int64 -> Int64 -> Int64
  den k p = foldl (\x acc -> (x*acc) `mod` p) 1 [1..k]
