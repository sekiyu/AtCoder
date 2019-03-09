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
  (n:m:c:_) <- fmap (map read . words) getLine :: IO [Int]
  bs <- fmap (map read . words) getLine :: IO [Int]
  am <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  print $ solve c bs am

solve c bs am = length $ filter (\as -> c + sum (zipWith (*) as bs) > 0) am 