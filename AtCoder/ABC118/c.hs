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
  n <- readLn :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve as

solve as = go $ IntSet.findMin aset
  where
    aset = IntSet.fromList as
    f m = IntSet.foldl' (\m' a -> if a `mod` m' == 0 then m' else a `mod` m') m aset
    go m = let m' = f m
      in if m' == m
         then m
         else go m'
