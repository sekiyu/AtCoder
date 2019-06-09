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
  ws <- map read . words <$> getLine :: IO [Int]
  print $ solve n ws

solve n ws = minimum $ map (\(s1, s2) -> abs $ (sum s2) - (sum s1)) [ splitAt i ws | i <- [1..n]]
  where 
    sorted = sort ws
