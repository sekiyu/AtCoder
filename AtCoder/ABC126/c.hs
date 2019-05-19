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
  (n:k:_) <- fmap (map read . words) getLine :: IO [Double]
  print $ solve n k

solve :: Double -> Double -> Double
solve n k = foldr (\i acc -> acc + prob i) 0 [1..n] 
  -- sum [ prob i | i <- [1..n]]
  where
    prob :: Double -> Double
    prob i
      | i > k = 1 / n
      | otherwise = 1 / (n * (go 1 i))
    go j i = if i * j >= k then j else go (2*j) i


