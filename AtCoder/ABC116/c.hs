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
  n <- readLn :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss

solve :: [Int] -> Int
solve hs 
  | all (==0) hs = 0
  | otherwise = 1 + (solve $ deleteSucceeding i m hs)
  where 
    (i, m) = maximumIndex hs
    deleteSucceeding :: Int -> Int -> [Int] -> [Int]
    deleteSucceeding i m hs = a ++ (delete b)
      where
        (a,b) = splitAt i hs
        delete [] = []
        delete (b:bs)
          | b == m = (b-1):(delete bs)
          | otherwise = (b:bs)
  
maximumIndex :: [Int] -> (Int, Int)
maximumIndex xs = go 0 0 0 xs
  where
    go _ i m [] = (i, m)
    go currentIndex i m (y:ys)
      | m < y = go (currentIndex + 1) currentIndex y ys
      | otherwise = go (currentIndex + 1) i m ys