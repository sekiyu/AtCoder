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
  n <- readLn :: IO Int
  ps <- map read . words <$> getLine :: IO [Int]
  print $ solve n ps

solve n ps = length . filter f $ zip3 ps (tail ps) (tail . tail $ ps)
  where
    f (x, y, z) = y /= mn && y /= mx
      where
        mn = minimum [x,y,z]
        mx = maximum [x,y,z]
