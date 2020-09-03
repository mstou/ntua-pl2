import Data.List (foldl', scanl')
import Data.Map.Strict (fromDistinctAscList)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Monoid
import Data.Int
import Control.Monad (replicateM)
import System.IO

maxElement = 1000001
-- sums = takeWhile (<maxElement) $ scanl1 (+) $ map (floor . (2**)) [0..]
sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

sum' p = foldl' (\acc x -> (acc + x) `mod` p) 0

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
                      sumOfHeads = ((sum' p) heads) `mod` p
                      tails = map tail lists
                      newSumTerms = if (addNewTerm $ x+1) then (dp : tails) else tails


getMapOfPrefixSums dp p = fromDistinctAscList $! prefixSumsWithKeys
  where prefixSumsWithKeys = take maxElement $ zip [0..] prefixSums
        prefixSums = scanl1 (\x y -> x `seq` (x+y) `mod` p) dp

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
  mapOfPrefixSums <- pure $ getMapOfPrefixSums (calculateDpList m) m
  answers <- pure $ map ( (answerQuerry mapOfPrefixSums m) . toTuple ) querries
  hPutBuilder stdout $ unlines_ $ map int64Dec (answers::[Int64])
    where
      unlines_ = mconcat . map (<> charUtf8 '\n')
      toTuple = (\(x:y:[]) -> (x,y))
