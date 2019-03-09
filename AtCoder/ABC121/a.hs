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
  (hh:ww:_) <- fmap (map read . words) getLine :: IO [Int]
  (h:w:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ solve h w hh ww

solve h w hh ww = (hh - h) * (ww - w)