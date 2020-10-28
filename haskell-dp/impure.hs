{-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -fstrictness #-}
import Data.Array
import Data.List (foldl', scanl')
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder
import System.IO as SIO
import Data.Monoid
import Data.Int

-- sums = takeWhile (<maxElement) $ scanl1 (+) $ map (floor . (2**)) [0..]
sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

dp i l p = result
  where
    previousIndices = takeWhile (>=0) $ map (\x -> i - x) sums
    result = foldl' (\acc x -> (acc + (l ! x)) `mod` p) 0 previousIndices

-- dp[1], dp[2], ...
calculateDpList p k = l
  where
    l = array (0,k) ((0,2) : (1,2) : [(i, dp i l p) | i <- [2..k]])

prefixSums l p k = a
  where
    a = array (0,k) ( (0, 1) : [(i, ((a ! (i-1)) + (l ! i)) `mod` p) | i <- [1..k]])

answerQuerry prefixSums p (0,y) = prefixSums ! y `mod` p
-- answerQuerry prefixSums p (1,y) = ((prefixSums ! y) - 1) `mod` p
answerQuerry prefixSums p (x,y) = ((prefixSums ! y) - (prefixSums ! (x-1))) `mod` p


readInts :: IO [Int64]
readInts = map parse . C.words <$> C.getLine
  where parse s = let Just (n, _) = C.readInteger s
                    in fromIntegral n

readAllQuerries :: IO [(Int,Int)]
readAllQuerries = allInts `seq` querries
  where parse s = let Just (n, _) = C.readInt s in n
        allInts = map parse . C.words <$> (C.hGetContents SIO.stdin)
        splitInts _ a b [] = (reverse a, reverse b)
        splitInts 0 a b (x:xs) = splitInts 1 (x:a) b xs
        splitInts 1 a b (x:xs) = splitInts 0 a (x:b) xs
        zipLists (a,b) = zip a b
        querries = zipLists . (splitInts 0 [] []) <$> allInts

main = do
  (n:m:[])        <- readInts
  querries        <- readAllQuerries
  maxElement      <- pure $ querries `seq` maximum $ map max' querries
  dpList          <- pure $ calculateDpList m (maxElement+1)
  prefixSumsArray <- pure $ maxElement `seq` prefixSums dpList m (maxElement+1)
  answers         <- pure $ prefixSumsArray `seq` map (answerQuerry prefixSumsArray m) querries
  hPutBuilder stdout $ unlines_ $ map int64Dec (answers::[Int64])
    where
      max' (a,b) = max a b
      unlines_ = mconcat . map (<> charUtf8 '\n')
