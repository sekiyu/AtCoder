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
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  s <- getLine
  putStrLn $ solve n k s

solve n k s = go k s
  where
    go _ [] = []
    go i (s:ss) = (if i == 1
                  then toLower s
                  else s) : (go (i-1) ss)
      