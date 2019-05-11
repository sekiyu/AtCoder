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
  ss <- replicateM n getLine :: IO [String]
  print $ solve ss

solve :: [String] -> Int
solve ss = (min endWithA startWithB) + orgAb 
  - if endWithA > 0 
      then if endWithA == startWithB && startWithB == aAndB then 1 else 0
      else 0
  where
    endWithA = sum $ map countA ss
    startWithB = sum $ map countB ss
    orgAb = sum $ map countAb ss
    aAndB = sum $ map countAandB ss

    countAb (s:[]) = 0
    countAb (s1:s2:s3) = (if s1 == 'A' && s2 == 'B' then 1 else 0) + countAb (s2:s3)
    countA s = if last s == 'A' then 1 else 0
    countB s = if head s == 'B' then 1 else 0
    countAandB s = if head s == 'B' && last s == 'A' then 1 else 0
