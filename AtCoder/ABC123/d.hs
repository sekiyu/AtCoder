-- ABC123 D
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
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents

main :: IO ()
main = do
  (x:y:z:k:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- readInts
  bs <- readInts
  cs <- readInts
  -- mapM_ print $ naive k as bs cs
  -- mapM_ print $ solve x y z k (sort as) (sort bs) (sort cs)
  mapM_ print $ solve x y z k as bs cs

solve x y z k as bs cs = comp (comp (takek as) (takek bs)) $ takek cs
  --comp (takek cs) $ comp (takek as) (takek bs)
  where
    takek = take k . reverse . sort
    naive'' k as bs = takek [ a + b | a <- as, b <- bs ]

    comp as bs = go [] as bs
      where
        go cs _ [] = cs
        go cs as (b:bs) = go (take k . merge cs $ map (+b) as) as bs

    merge :: (Ord a) => [a] -> [a] -> [a]
    merge as [] = as
    merge [] bs = bs
    merge (a:as) (b:bs)
      | a >= b = a:(merge as (b:bs))
      | a < b  = b:(merge (a:as) bs)
      
    naive' k as bs = takek $ do
      a <- as 
      b <- bs 
      return $ a + b


mergeSort :: (Ord a) => [a] -> [a]
mergeSort (a:[]) = [a]
mergeSort as = (mergeSort left) `merge` (mergeSort right)
  where
    (left, right) = splitAt m as
    m = length as `div` 2
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge as [] = as
    merge [] bs = bs
    merge (a:as) (b:bs)
      | a <= b = a:(merge as (b:bs))
      | a > b  = b:(merge (a:as) bs)
      

naive k as bs cs = take k . reverse . sort $ do
  a <- as 
  b <- bs 
  c <- cs
  return $ a + b + c