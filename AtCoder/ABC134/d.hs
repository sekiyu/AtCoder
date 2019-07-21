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
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  solve' n as

solve' :: Int -> [Int] -> IO ()
solve' n as = do
  let mp = foldl' go IntMap.empty . reverse $ zip as [1..n]
      m = sum . IntMap.elems $ mp
  print m
  if m == 0 
    then return ()
    else putStrLn . unwords . map show $ IntMap.keys mp
  where
    arr = listArray (1, n) as :: Array Int Int
    go :: IntMap.IntMap Int -> (Int, Int) -> IntMap.IntMap Int
    go mp (!a, !i) = newmp
      where
        bi = ((sumbs `mod` 2) + arr!i) `mod` 2
        newmp = if bi /= 0
          then IntMap.insert i bi mp
          else mp
        sumbs = sum $ catMaybes [ IntMap.lookup (i * j) mp | j <- [2..(n `div` i)]]

  
  

solve :: Int -> [Int] -> IO ()
solve n as = go (IntMap.empty) . reverse $ zip as [1..n]
  where
    arr = listArray (1, n) as :: Array Int Int
    go :: IntMap.IntMap Int -> [(Int, Int)] -> IO ()
    go mp [] = do
      let m = sum . IntMap.elems $ mp
      print m
      if m == 0 
        then return ()
        else putStrLn . unwords . map show $ IntMap.keys mp
    go mp ((a, i):as) = go newmp as
      where
        bi = ((sumbs `mod` 2) + arr!i) `mod` 2
        newmp = if bi /= 0
          then IntMap.insert i bi mp
          else mp
        sumbs = sum $ catMaybes [ IntMap.lookup (i * j) mp | j <- [2..(n `div` i)]]


