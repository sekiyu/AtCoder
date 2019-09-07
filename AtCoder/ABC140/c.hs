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
  bs <- map read . words <$> getLine :: IO [Int]
  print $ solve n bs

solve n bs = a0 + mid + aLast
  where
    a0 = head bs
    aLast = last bs
    mid = sum $ zipWith min bs (tail bs)