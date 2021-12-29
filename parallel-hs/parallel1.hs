import Binomial
import Data.Int
import Control.Monad
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Typeable

deep :: NFData a => a -> a
deep a = deepseq a a

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
  let solutions = deep $ parMap rpar solve queries
  mapM print solutions
