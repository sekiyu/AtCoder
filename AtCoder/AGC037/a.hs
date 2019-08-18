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
main = getLine >>= print . solve
solve :: String -> Int
solve = go 0
  where
    go !acc (s1:s2:s3:ss) = if s1 == s2
        then go (acc + 2) ss
        else go (acc + 1) (s2:s3:ss)
    go !acc (s1:s2:[]) = acc + if s1 == s2 then 1 else 2
    go !acc (s:[]) = acc + 1
    go !acc [] = acc
