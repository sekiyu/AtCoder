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
  (n:q:_) <- map read . words <$> getLine :: IO [Int]
  s <- getLine :: IO String
  tds <- replicateM q $ (\(a:b:_) -> (head a, head b)) . words <$> getLine :: IO [(Char, Char)]
  print $ solve n s tds

solve n s tds = rbound - lbound - 1
  where
    rbound = if vanishRight n 
             then lowerBound vanishRight (1, n) 
             else n + 1
    lbound = if vanishLeft 1 
             then upperBound vanishLeft (1, n) 
             else 0

    sarr = listArray (1, n) s
    vanish left right i = go i tds
      where
        go :: Int -> [(Char, Char)] -> Bool
        go i tds
          | i > n = right
          | i < 1 = left
          | null tds = False
          | otherwise = if sarr ! i == t
                        then if d == 'R'
                             then go (i+1) $ tail tds
                             else go (i-1) $ tail tds
                        else go i $ tail tds
            where (t, d) = head tds

    vanishRight = vanish False True
    vanishLeft = vanish True False
          
lowerBound :: (Int -> Bool) -> (Int, Int) -> Int
lowerBound predicate bounds = go bounds
  where
    go (l, h) | l + 1 == h = if predicate l then l else h
              | predicate m = go (l, m)
              | otherwise = go (m, h)
      where m = (l + h) `div` 2
    
upperBound :: (Int -> Bool) -> (Int, Int) -> Int
upperBound predicate bounds = go bounds
  where
    go (l, h) | l + 1 == h = if predicate h then h else l
              | predicate m = go (m, h)
              | otherwise = go (l, m)
      where m = (l + h) `div` 2
    