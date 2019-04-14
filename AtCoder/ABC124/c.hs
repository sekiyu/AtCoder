-- ABC124 C
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

import Data.Char

main :: IO ()
main = getLine >>= print . solve

solve :: String -> Int
solve s = min (calc wb) (calc bw)
  where
    ints = map digitToInt s :: [Int]
    wb = cycle [1, 0]
    bw = cycle [0, 1]
    calc = sum . map abs . zipWith (-) ints