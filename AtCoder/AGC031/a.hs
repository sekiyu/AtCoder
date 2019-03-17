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
  n <- readLn :: IO Int
  s <- getLine :: IO String
  print $ solve s
 
solve s = (Map.foldr (\a acc -> (a `modadd` 1) `modmul` acc) 1 cs) - 1
  where
    cs = toCountMap s

toCountMap :: (Ord k) => [k] -> Map.Map k Integer
toCountMap = Map.fromListWith (+) . flip zip (repeat 1)

divConst = 10^9 + 7 :: Integer
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
