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
  (a:b:c:_) <- fmap (map read . words) getLine :: IO [Int]
  putStrLn $ if solve a b c then "Yes" else "No"

solve a b c = and [a == b, b == c]

isTriangle a b c = and [a + b > c, b + c > a, c + a > b]