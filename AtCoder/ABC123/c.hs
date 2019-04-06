-- ABC123 C
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
  ss <- replicateM 6 $ readLn :: IO [Int]
  print $ solve ss

solve ss = 5 + groups - 1 
  where
    n = head ss
    as = tail ss
    mn = minimum as 
    groups = n `div` mn + if n `mod` mn == 0 then 0 else 1