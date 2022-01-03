import Binomial
import Data.Int
import Control.Monad
import Control.Monad.Par
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Typeable

deep :: NFData a => a -> a
deep a = deepseq a a

solve :: [Int64] -> Par Int64
solve ([n,k,p]) = do
  _nominator <- spawnP $ nom n k p
  _denominator <- spawnP $ den k p
  denominator <- get _denominator
  _inverse_den <- spawnP $ inverse denominator p
  nominator <- get _nominator
  inverse_den <- get _inverse_den
  return $ (nominator * inverse_den) `mod` p

main = do
  q <- readLn
  allLines <- replicateM q getLine
  let queries = ((map read) . words) <$> allLines :: [[Int64]]
  -- let forcing_serial_part = deep $ length queries
  let solutions = deep $ runPar $ parMapM solve queries
  mapM print solutions
