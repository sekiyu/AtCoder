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
  (n:k:_) <- map read . words <$> getLine :: IO [Int]
  as <- map read . words <$> getLine :: IO [Integer]
  print $ solve n k as

solve :: Int -> Int -> [Integer] -> Integer
solve n k as = go 0 0 sums $ filter (intk<=) sums
  where
    sums = scanl' (+) 0 as
    intk = fromIntegral k
    go :: Integer -> Integer -> [Integer] -> [Integer] -> Integer
    go s _ _ [] = s
    go s m (b:bs) (c:cs) = if c - b < intk 
      then go (s + m) m (b:bs) cs
      else go s (m + 1) bs (c:cs) 
