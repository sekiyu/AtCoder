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
  xys <- replicateM n $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  print $ solve n xys

solve n xys 
  | n == 1 = 1
  | otherwise = n - (maximum . Map.elems . toCountMap $ distances xys)

distances xys = nEq ++ eq
  where
    nEq = do
      (ax, ay) <- xys
      (bx, by) <- xys
      guard $ ax < bx
      return (ax - bx, ay - by)
    eq = do
      (ax, ay) <- xys
      (bx, by) <- xys
      guard $ ax == bx
      guard $ ay < by
      return (ax - bx, ay - by)

toCountMap :: (Ord k) => [k] -> Map.Map k Int
toCountMap xs = Map.fromListWith (+) $ zip xs (repeat 1)
