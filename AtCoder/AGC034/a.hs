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
-- import Data.Array
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST

main :: IO ()
main = do
  (n:a:b:c:d:_) <- fmap (map read . words) getLine :: IO [Int]
  s <- getLine
  print $ solve a b c d s

solve a b c d s 
  | c < d = simple
  | otherwise = simple && passable
  where
    road = listArray (1, length s) s
    simple = reachable a c && reachable b d
    reachable x y = drop (x-1) . take y s
    proceedable s:[] = True
    proceedable (s:ss:sss)
      | s == "#" && ss == "#" = False
      | otherwise = proceedable (ss:sss)

    passable = True

