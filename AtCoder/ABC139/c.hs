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

main :: IO ()
main = do
  n <- readLn :: IO Int
  getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve = (\(a, b, _) -> max a b) . foldl' go (0, 0, 0) 
  where
    go :: (Int, Int, Int) -> Int -> (Int, Int, Int)
    go (!mx, !current, !prev) h
      | prev >= h = (mx, current + 1, h)
      | otherwise = (max mx current, 0, h)
