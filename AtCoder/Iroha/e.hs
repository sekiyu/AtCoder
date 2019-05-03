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
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]

main :: IO ()
main = do
  (n:a:b:_) <- fmap (map read . words) getLine :: IO [Integer]
  ds <- if b == 0 
        then return []
        else readIntegers :: IO [Integer]
  print $ solve' n a b $ sort ds

solve' :: Integer -> Integer -> Integer -> [Integer] -> Integer
solve' n a b ds = n - (h + l + (foldl' f 0 $ zip ds (tail ds)) + b)
  where
    f :: Integer -> (Integer, Integer) -> Integer
    f !acc (prev, next) = acc + (next - prev - 1) `div` a
    h = if null ds then 0 else (head ds - 1) `div` a
    l = if null ds then n `div` a else (n - last ds) `div` a

solve n a b ds = n - (go 0 1 ds)
  where
    go !num_date !i []
     | i + a - 1 > n = num_date
     | otherwise = go (num_date + 1) (i + a) []
    go !num_date !i (d:ds)
      | i == d = go (num_date + 1) (i + 1) ds
      | i + a > d = go num_date d (d:ds)
      | otherwise = go (num_date + 1) (i + a) (d:ds)





