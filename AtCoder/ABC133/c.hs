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
main = getLine >>= print . solve . map read . words
solve :: [Int] -> Int
solve (l:r:_) 
  | r - l >= 2019 = 0
  | otherwise = minimum [ (i * j) `mod` 2019 | i <- [l..(r-1)], j <- [(i+1)..r] ]
