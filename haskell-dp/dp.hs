import Data.List (foldl', scanl')
import Data.Map.Strict (fromDistinctAscList)
import qualified Data.Map.Strict as Map

p = 1000000007
maxElement = 1000001
-- sums = takeWhile (<maxElement) $ scanl1 (+) $ map (floor . (2**)) [0..]
sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

sum' = foldl' (\acc x -> (acc + x) `mod` p) 0

addNewTerm x = x `elem` sums

-- dp[1], dp[2], ...
dp = 2 : 2 : sumHeads 2 [tail dp]
  where
    sumHeads x lists =
      heads `seq` sumOfHeads `seq` tails `seq` newSumTerms `seq`
      (sumOfHeads : sumHeads (x+1) newSumTerms)
        where heads = map head lists
              sumOfHeads = (sum' heads) `mod` p
              tails = map tail lists
              newSumTerms = if (addNewTerm $ x+1) then (dp : tails) else tails


mapOfPrefixSums = fromDistinctAscList $! prefixSumsWithKeys
  where prefixSumsWithKeys = take maxElement $ zip [0..] prefixSums
        prefixSums = scanl1 (\x y -> x `seq` (x+y) `mod` p) dp

answerQuerry x y =
  prefixSumX `seq` prefixSumY `seq` (prefixSumY - prefixSumX) `mod` p
  where (Just prefixSumX) = if x == 0 then (Just 1) else Map.lookup (x-1) mapOfPrefixSums
        (Just prefixSumY) = Map.lookup y mapOfPrefixSums

main = do
  print(answerQuerry 0 10)
  print(answerQuerry 1742 4217)
  print(answerQuerry 500000 999999)
  -- mapOfPrefixSums `seq` print (dp !! 1000000)
