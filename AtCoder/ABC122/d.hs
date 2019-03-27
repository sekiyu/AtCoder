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
  print $ solve n

-- solve :: Int -> Integer
solve n = length $ [ w | w <- replicateM n acgt, not . or . map (flip isInfixOf w) $ ngwords ]

acgt = "ACGT"
ngwords = ["AGC", "GAC", "ACG", "ACGC", "AGGC", "ATGC", "AGAC", "AGGC", "AGTC"]