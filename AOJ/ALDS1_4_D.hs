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
import Data.Array.IArray

main :: IO ()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  ws <- replicateM n $ readLn :: IO [Int]
  print $ solve k ws

solve :: Int -> [Int] -> Int
solve k ws = lowerBound (isPackable k ws) (m - 1, mx)
  where
    m = maximum ws
    mx = sum ws

-- 条件を満たす最初の要素を指すインデックスを返す
-- lowerBound :: (Ord a, Ix a) => (a -> Bool) -> (a, a) -> a
lowerBound :: (Int -> Bool) -> (Int, Int) -> Int
lowerBound predicate bounds = go bounds
  where
    go (l, h) | l + 1 == h = h
              | predicate m = go (l, m)
              | otherwise = go (m, h)
      where m = (l + h) `div` 2

isPackable :: Int -> [Int] -> Int -> Bool
isPackable k ws p = k >= (snd $ foldl' f (0, 1) ws)
  where
    f :: (Int, Int) -> Int -> (Int, Int)
    f (i, acc) w = if i + w <= p
                   then (i + w, acc)
                   else (w, acc + 1)


{-
lowerBound predicate bounds = go m bounds 
  where
    (m, mx) = bounds
    go :: Int -> (Int, Int) -> Int
    go !i !(!im, !imx) 
      | i == m = if predicate m then m else go (m+1) (m + 1, imx)
      | otherwise = if predicate i
                    then if predicate (i-1)
                         then go ((im + i) `div` 2) (im, i - 1)
                         else i
                    else go ((imx + i) `div` 2 + 1) (i, imx)
-}


{-
numOfPackage k ws p = go 0 ws
  where
    go :: Int -> [Int] -> Int
    go _ [] = 1
    go i (w:ws) = if i + w <= p
                  then go (i+w) ws
                  else 1 + go w ws
-}