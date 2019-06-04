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
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  vs <- map read . words <$> getLine :: IO [Int]
  print $ solve n k vs

solve n k vs = maximum $ map cost [0..(min n k)]
  where
    cost i = maximum [ 
      sum . deleteNegative (k-i) . sort $ (take j vs) ++ (drop (n - i + j) vs) | j <- [0..i]]
    deleteNegative l [] = []
    deleteNegative l (a:as) 
      | l <= 0 = a:as 
      | otherwise = if a < 0 
                    then deleteNegative (l-1) as
                    else (a:as)