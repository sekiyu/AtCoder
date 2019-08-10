{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
import Data.Array
-- import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IO
import Data.Char
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents
readVInts = map (fst . fromJust . B.readInt) . B.lines <$> B.getContents

main :: IO ()
main = do
  (n:q:_) <- map read . words <$> getLine :: IO [Int]
  -- qs <- read2dInts
  qs <- replicateM q $ map read . words <$> getLine :: IO [[Int]]
  solve n qs

solve :: Int -> [[Int]] -> IO ()
solve n qs = do
  arr <- newArray (0, n) [] :: IO (IOArray Int [Int])
  forM_ qs $ \(query:t:qss) -> do
    at <- readArray arr t :: IO [Int]
    case query of
      0 -> do
        let ([x]) = qss 
        writeArray arr t (x:at) :: IO () 
        return arr
      1 -> do
        if null at
          then return ()
          else print $ head at
        return arr
      2 -> do
        if null at
          then return ()
          else writeArray arr t $ tail at
        return arr
        