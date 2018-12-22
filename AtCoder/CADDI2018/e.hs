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
solve ss = minimum [ calc $ splitAt i ss | i <- [0..n-1]]
  where
    n = length ss
    calc (xs,ys) = length xs + numOp (reverse xs) + numOp ys

numOp [] = 0
numOp (x:[]) = 0  
numOp (x1:x2:xs) 
  | x1 <= x2 = numOp (x2:xs)
  | otherwise = 2 + numOp (x1:4*x2:xs)

