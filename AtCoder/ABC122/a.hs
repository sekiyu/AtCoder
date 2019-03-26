-- ABC122 A
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


main = getLine >>= putStrLn . solve
solve :: String -> String
solve b = case b of
  "A" -> "T"
  "T" -> "A"
  "C" -> "G"
  "G" -> "C"