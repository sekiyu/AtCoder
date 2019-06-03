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
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  kss <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  ps <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n kss ps

solve :: Int -> [[Int]] -> [Int] -> Int
solve n kss ps = length $ filter allOn candidates
  where
    candidates = replicateM n [0, 1] :: [[Int]]
    allOn :: [Int] -> Bool
    allOn switches = all id . zipWith (==) ps $ map (f switches) kss
    f :: [Int] -> [Int] -> Int
    f switches (k:ss) = (sum $ map (sarr!) ss) `mod` 2
      where
        sarr = listArray (1, n) switches

