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
solve (a:b:c:d:_)
  | a == b && b /= c && c == d = "Yes"
  | a == c && b /= c && b == d = "Yes"
  | a == d && d /= c && b == c = "Yes"
  | otherwise = "No"
