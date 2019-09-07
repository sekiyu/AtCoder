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
  as <- map read . words <$> getLine :: IO [Int]
  bs <- map read . words <$> getLine :: IO [Int]
  cs <- map read . words <$> getLine :: IO [Int]
  print $ solve n as bs cs

solve :: Int -> [Int] -> [Int] -> [Int] -> Int
solve n as bs cs = sum bs + (fst $ foldl' f (0, -1) as)
  where
    -- b = listArray (1, n) bs
    c = listArray (1, n-1) cs

    f :: (Int, Int) -> Int -> (Int, Int)
    f (!acc, !prev) a 
      -- | a == 1 = (acc, a)
      = (acc + if prev+1 == a then c!(a-1) else 0, a)