-- Maximum cup 2018 D - Many Go Round
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Array

main :: IO ()
main = do
  (n:m:l:x:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  putStrLn $ if solveDp n m l x as then "Yes" else "No"

solveDp :: Int -> Int -> Int -> Int -> [Int] -> Bool
solveDp n m l x as = traceShow dp $ dp!(n, l) <= x
  where
    arr = listArray (0, n) (0:as)
    dp = listArray ((0, 0), (n, m))
      $ map f [ (i, j)| i <- [0..n], j <- [0..m]] :: Array (Int, Int) Int
    f :: (Int, Int) -> Int
    f (_, 0) = 1
    f (0, _) = maxBound `div` 2
    f (i, j) = min (dp!(i-1, j))
      $ dp!(i-1, (j - arr!i) `mod` m) + (j + arr!i) `div` m


solveIntMap :: Int -> Int -> Int -> Int -> [Int] -> Bool
solveIntMap n m l x as = IntMap.member l dp && dp IntMap.! l <= x
  where
    dp = foldr f (IntMap.singleton 0 1) as
    f :: Int -> IntMap.IntMap Int -> IntMap.IntMap Int
    f a dic = IntMap.foldrWithKey addIfFirst dic dic
      where
        addIfFirst k v d = let new = (k + a) `mod` m
                           in if IntMap.member new d
                           then IntMap.insertWith min new (v + (k + a) `div` m) d
                           else IntMap.insert new (v + (k + a) `div` m) d

-- definitive solution
solve :: Int -> Int -> Int -> Int -> [Int] -> Bool
solve n m l x as = any (\s -> s `mod` m == l)
  . filter (<= m*x) . map sum $ subsequences as
