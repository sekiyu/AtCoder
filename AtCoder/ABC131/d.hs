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
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents

main :: IO ()
main = do
  n <- readLn :: IO Int
  -- abs <- replicateM n $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  abs <- map (\(a:b:_) -> (a,b)) <$> read2dInts :: IO [(Int, Int)]

  putStrLn $ if solve abs then "Yes" else "No"

solve :: [(Int, Int)] -> Bool
solve abs = all (\(x, y) -> x <= y) $ scanl' f (0, 0) sorted
  where
    sorted = sortOn snd abs
    f :: (Int, Int) -> (Int, Int) -> (Int, Int)
    f (a, b) (x, y) = (a + x, y)