-- ABC122 B
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

main = getLine >>= print . solve
solve :: String -> Int
solve = go 0 0
  where
    go :: Int -> Int -> String -> Int
    go m i [] = max m i
    go m i (s:ss) = if elem s acgt 
                    then go m (i+1) ss
                    else go (max m i) 0 ss


acgt = "ACGT"