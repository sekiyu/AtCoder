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
  (h:w:_) <- fmap (map read . words) getLine :: IO [Int]
  am <- replicateM h getLine :: IO [[Char]]
  putStrLn . unlines $ solve h w am

solve :: Int -> Int -> [[Char]] -> [[Char]]
solve h w am = [[a!(i,j) | j <- IntSet.elems ws] | i <- IntSet.elems hs]
  where
    a :: Array (Int, Int) Char
    a = listArray ((1,1), (h, w)) $ join am
    (hs,ws) = foldr f (IntSet.empty, IntSet.empty) [(i,j) | i<-[1..h], j<-[1..w]]
    f (i,j) (x,y) = if a!(i,j) == '#'
                    then (IntSet.insert i x, IntSet.insert j y)
                    else (x, y)
