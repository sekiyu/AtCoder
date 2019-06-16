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
  (n:x:_) <- map read . words <$> getLine :: IO [Int]
  ls <- map read . words <$> getLine :: IO [Int]
  print $ solve n x ls

solve n x ls = length . filter (<=x) $ scanl' (+) 0 ls