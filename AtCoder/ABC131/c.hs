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
solve :: [Integer] -> Integer
solve (a:b:c:d:_) = nTotal - nc - nd + ncd
  where
    nTotal = b - a + 1
    nc = b `div` c - (a - 1) `div` c
    nd = b `div` d - (a - 1) `div` d
    ncd = b `div` (lcm c d) - (a - 1) `div` (lcm c d)