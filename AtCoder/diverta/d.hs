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
main = readLn >>= print . solve'

solve :: Integer -> Integer
solve n = sum [ i | i <- [2..(n-1)], n `mod` i == n `div` i]

solve' n = if null ans then 0 else sum ans
  where
    ans = catMaybes . map calc $ [1..mx]
    mx = ceiling . sqrt $ fromIntegral n
    calc m 
      | x == 0 = Nothing
      | (n - m) `mod` m == 0 && n `mod` x == n `div` x = Just x
      | otherwise = Nothing
      where x = (n - m) `div` m