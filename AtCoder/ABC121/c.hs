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
  abs <- replicateM n $ (\(a:b:_) -> (read a, read b)) . words <$> getLine :: IO [(Integer, Int)]
  print $ solve m abs

solve m abs = sum . take m . join . map (\(a, b) -> replicate b a) $ sortOn fst abs