import Binomial
import Data.Int
import Control.Monad

solve ([n,k,p]) = binom
  where
    nominator = nom n k p
    denominator = den k p
    inverse_den = inverse denominator p
    binom = (nominator * inverse_den) `mod` p

main = do
  q <- readLn
  allLines <- replicateM q getLine
  let queries = ((map read) . words) <$> allLines :: [[Int64]]
  mapM (print . solve) queries
