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

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  abs <- replicateM n $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  xs <- replicateM n getLine :: IO [String]
  ss <- fmap (map read . words) getLine :: IO [Int]
  -- ps <- fmap join . replicateM n $ map read . words <$> getLine :: IO [Int]
  ps <- replicateM n $ readLn :: IO [Int]

  print $ solve ss
  putStrLn . unwords . map show $ solve ss -- print list in 1 line
  putStrLn . unlines . map show $ solve ss -- print list vertically


main = getLine >>= putStrLn . solve . map read . words
solve :: [Int] -> String

import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents


import qualified Data.ByteString.Char8 as B
main = B.getContents >>= print . B.count '\n'


-- ByteString functions
B.getContents :: IO B.ByteString
B.lines :: B.ByteString -> [B.ByteString]
B.words :: B.ByteString -> [B.ByteString]
B.readInt :: B.ByteString -> Maybe (Int, B.ByteString)
