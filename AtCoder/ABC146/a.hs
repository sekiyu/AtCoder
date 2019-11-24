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
main = getLine >>= print . solve
solve :: String -> Int
solve s = go s days
  where
    go s (d:ds)
      | s == d = (length ds) + 1
      | otherwise = go s ds


days = ["SUN","MON","TUE","WED","THU","FRI","SAT"]