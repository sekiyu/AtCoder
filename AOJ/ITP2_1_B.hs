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
import qualified Data.Sequence as S

import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents
readVInts = map (fst . fromJust . B.readInt) . B.lines <$> B.getContents

main :: IO ()
main = do
  q <- readLn :: IO Int
  -- qs <- read2dInts
  qs <- replicateM q $ map read . words <$> getLine :: IO [[Int]]
  solve qs

solve :: [[Int]] -> IO()
solve = foldM_ f S.empty
  where
    f :: S.Seq Int -> [Int] -> IO (S.Seq Int)
    f s (query:qs) 
      | query == 0 = push s qs
      | query == 1 = randomAccess s qs
      | query == 2 = pop s qs
    push s (d:x:_)
      | d == 0 = return $ x S.<| s
      | d == 1 = return $ s S.|> x
    randomAccess s (p:_) = do
      print $ fromJust $ S.lookup p s
      return s
    pop s (d:_)
      | d == 0 = let (x S.:<| ss) = s in return ss
      | d == 1 = let (ss S.:|> x) = s in return ss


