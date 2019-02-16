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
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  kas <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  print $ solve m kas

solve m kas = length . filter id $ map everyone [1..m]
  where
    as = map tail kas
    everyone i = all (\a -> i `elem` a) as