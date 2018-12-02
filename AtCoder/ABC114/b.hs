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

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = minimum cs
  where
    cs = map (\x -> abs (x - 753))
      $ filter (>110) [ n `div` m `mod` 1000 | m <- (map (10^) [0..10]) ]
