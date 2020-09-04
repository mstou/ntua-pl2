import Data.List (foldl', scanl')
import Control.Monad (replicateM)
import Data.Map.Strict (fromDistinctAscList)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder
import Data.Monoid
import Data.Int
import System.IO

-- sums = takeWhile (<maxElement) $ scanl1 (+) $ map (floor . (2**)) [0..]
sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

sum' = foldl' (+) 0

addNewTerm x = x `elem` sums

-- dp[1], dp[2], ...
calculateDpList p = dp
  where
        dp = 2 : 2 : sumHeads 2 [tail dp]
          where
            sumHeads x lists =
              heads `seq` sumOfHeads `seq` tails `seq` newSumTerms `seq`
              (sumOfHeads : sumHeads (x+1) newSumTerms)
                where heads = map head lists
                      sumOfHeads = (sum' heads) `mod` p
                      tails = map tail lists
                      newSumTerms = if (addNewTerm $ x+1) then (dp : tails) else tails

getMapOfPrefixSums p maxElement =
  dp `seq` prefixSums `seq` prefixSumsWithKeys `seq`
  fromDistinctAscList $! prefixSumsWithKeys
  where prefixSumsWithKeys = tail $ zip [-1..] prefixSums
        prefixSums = scanl' (\x y -> (x+y) `mod` p) 0 dp
        dp = take (fromIntegral maxElement) (calculateDpList p)

answerQuerry mapOfPrefixSums p (x,y) =
  mapOfPrefixSums `seq` prefixSumX `seq` prefixSumY `seq` (prefixSumY - prefixSumX) `mod` p
  where (Just prefixSumX) = if x == 0 then (Just 1) else Map.lookup (x-1) mapOfPrefixSums
        (Just prefixSumY) = Map.lookup y mapOfPrefixSums

readInts :: IO [Int64]
readInts = map parse . C.words <$> C.getLine
  where parse s = let Just (n, _) = C.readInteger s
                    in fromIntegral n
main = do
  (n:m:[]) <- readInts
  querries <- replicateM (fromIntegral n) readInts
  maxElement <- pure $ querries `seq` maximum $ map maximum querries
  mapOfPrefixSums <- pure $ maxElement `seq` getMapOfPrefixSums m (maxElement+1)
  answers <- pure $ mapOfPrefixSums `seq` map ( (answerQuerry mapOfPrefixSums m) . toTuple ) querries
  answers `seq` hPutBuilder stdout $ unlines_ $ map int64Dec (answers::[Int64])
    where
      unlines_ = mconcat . map (<> charUtf8 '\n')
      toTuple = (\(x:y:[]) -> (x,y))
