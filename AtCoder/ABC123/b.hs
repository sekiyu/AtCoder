-- ABC123 B
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
  ss <- replicateM 5 $ readLn :: IO [Int]
  print $ solve ss

solve ss = (sum ups) - maximum diffs
  where
    ups = map roundup ss
    diffs = zipWith (-) ups ss

roundup s = go 0 s
  where
    go t s | t >= s = t
           | otherwise = go (t+10) s