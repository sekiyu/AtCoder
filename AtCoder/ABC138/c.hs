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
main = do
  n <- readLn :: IO Int
  vs <- map read . words <$> getLine :: IO [Double]
  print $ solve n (sort vs)

halve x = x / 2.0

solve n (v:vs) = go v vs
  where
    go acc (v0:[]) = halve (acc + v0)
    go acc (v:vs) = go (halve $ acc + v) vs