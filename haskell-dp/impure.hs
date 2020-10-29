{-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -fstrictness #-}
import Data.Int
import Data.Array.ST
import Data.Array.Unboxed
import Data.Monoid
import System.IO as SIO
import Data.ByteString.Builder
import Data.List (foldl', scanl')
import Control.Monad.ST
import Control.Monad (replicateM, forM_, mapM, when)
import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as C
import qualified Data.Array.MArray as MArray

sum' = foldl' (+) 0

-- sums = takeWhile (<maxElement) $ scanl1 (+) $ map (floor . (2**)) [0..]
sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

-- dp[1], dp[2], ...
calculateDpList p k = dpArray
  where
    dpArray = runSTUArray $ do
      arr <- MArray.newArray (0, k) (fromIntegral (-1))
      writeArray arr 0 2
      writeArray arr 1 2
      forM_ [2..k] $ \i -> do
        val <- readArray arr i
        when (val == -1) $ do
          let previousIndices = takeWhile (>=0) $ map (\x -> i - x) sums
          values <- mapM (readArray arr) previousIndices
          let newValue = (sum' values) `mod` p
          writeArray arr i newValue
      return arr

prefixSums :: (UArray Int Int64) -> Int64 -> Int -> (UArray Int Int64)
prefixSums l p k = prefixSumsArray
  where
    prefixSumsArray = runSTUArray $ do
      arr <- MArray.newArray (0,k) (fromIntegral (-1))
      writeArray arr 0 1
      forM_ [1..k] $ \i -> do
        val <- readArray arr i
        when (val == -1) $ do
          prev <- readArray arr (i-1)
          let newValue = l ! i
          let newSum = (prev + newValue) `mod` p
          writeArray arr i newSum
      return arr

answerQuerry prefixSums p (0,y) = prefixSums ! y `mod` p
answerQuerry prefixSums p (x,y) = ((prefixSums ! y) - (prefixSums ! (x-1))) `mod` p


readInts :: IO [Int64]
readInts = map parse . C.words <$> C.getLine
  where parse s = let Just (n, _) = C.readInteger s
                    in fromIntegral n

readAllQuerries :: IO [(Int,Int)]
readAllQuerries = allInts `seq` querries
  where parse s = let Just (n, _) = C.readInt s in n
        allInts = map parse . C.words <$> (C.hGetContents SIO.stdin)
        split l =
          let
          (a, b, _) = foldr
                       (\x (a, b, t) -> if t == 1 then (a, x:b, 0) else (x:a, b,1))
                       ([], [], 1)
                       l
          in
            zip a b
        querries = split <$> allInts

main = do
  (n:m:[])        <- readInts
  querries        <- readAllQuerries
  maxElement      <- pure $ querries `seq` maximum $ map max' querries
  dpList          <- pure $ maxElement `seq` calculateDpList m (maxElement+1)
  prefixSumsArray <- pure $ dpList `seq` prefixSums dpList m (maxElement+1)
  answers         <- pure $ prefixSumsArray `seq` map (answerQuerry prefixSumsArray m) querries
  answers `seq` hPutBuilder stdout $ unlines_ $ map int64Dec (answers::[Int64])
    where
      max' (a,b) = max a b
      unlines_ = mconcat . map (<> charUtf8 '\n')
