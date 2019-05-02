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
  (n:m:k:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve' n m k as

ninf = -10^10

solve n m k as = let ans = maximum [ dp!(prev, m) | prev <- [(n - k + 1)..n], prev >= 0]
                 in if ans < 0 then -1 else ans
  where
    arr = listArray (1, n) as
    bounds = ((0, 0), (n, m))
    dp = listArray bounds $ map f $ range bounds
    f :: (Integer, Integer) -> Integer
    f (0, 0) = 0
    f (_, 0) = ninf
    f (0, _) = ninf
    f (i, j) = arr!i + maximum [ dp!(prev, j - 1) | prev <- [(i - k)..(i-1)], prev >= 0]
    
solve' n m k as = let ans = maximum [ dp!i | i <- [(n - k + 1)..n], i >= 0]
                  in if ans < 0 then -1 else ans
  where
    arr = listArray (1, n) as
    initial = listArray (0, n) $ 0:(replicate n ninf)
    dp = foldl' f initial [1..m]
    f :: Array Int Integer -> Int -> Array Int Integer
    f prev _ = listArray (0, n) $ map g [0..n]
      where
        g 0 = ninf
        g i = arr!i + maximum [ prev!j | j <- [(i - k)..(i - 1)], j >= 0]
