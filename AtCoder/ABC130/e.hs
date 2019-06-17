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
  (n:m:_) <- map read . words <$> getLine :: IO [Int]
  ss <- map read . words <$> getLine :: IO [Int]
  ts <- map read . words <$> getLine :: IO [Int]
  print $ solve n m ss ts

solve n m ss ts = traceShow dp $ sum . elems $ dp
  where
    s = listArray (1, n) ss
    t = listArray (1, m) ts
    dp = listArray ((1, 1), (n, m)) $ [ f i j | i <- [1..n], j <- [1..m]]
    sums = listArray ((0, 0), (n, m)) $ [ g i j | i <- [0..n], j <- [0..m]]
    f i j = if s!i == t!j 
      then sums!(i - 1, j - 1) `modadd` 1 
      else 0
    g 0 0 = 1
    g 0 _ = 0
    g _ 0 = 0
    g i j = (sums!(i-1, j)) `modadd` (sums!(i, j-1)) `modsub` (sums!(i-1, j-1)) `modadd` (dp!(i, j))


divConst = 10^9 + 7 :: Integer
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst
    

rowdp :: String -> String -> Int
rowdp xs ys = last $ foldl' scanY headRow xs
  where
    m = length ys
    headRow = replicate (m + 1) 0 :: [Int]
    scanY :: [Int] -> Char -> [Int]
    scanY prevRow x = scanl' f 0 yvprev
      where
        yvprev = zip3 ys prevRow $ tail prevRow
        f :: Int -> (Char, Int, Int) -> Int
        f v (y, a, b) = if x == y 
                        then 1 + a
                        else max b v
 
