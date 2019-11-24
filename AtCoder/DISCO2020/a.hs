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
main = getLine >>= print . solve . map read . words
solve :: [Int] -> Int
solve (x:y:_) = additional + (prize x) + (prize y)
  where
    additional = if x == 1 && y == 1 then 400000 else 0
    prize 1 = 300000
    prize 2 = 200000
    prize 3 = 100000
    prize _ = 0
