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
main = getLine >>= putStrLn . solve

solve :: String -> String
solve s 
  | lmm && rmm = "AMBIGUOUS"
  | lmm = "MMYY"
  | rmm = "YYMM"
  | otherwise = "NA"
  where
    (l, r) = splitAt 2 s
    lmm = canBeMM $ read l
    rmm = canBeMM $ read r

canBeMM n = 1 <= n && n <=12 

