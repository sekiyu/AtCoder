-- TDPC D - サイコロ
{-# LANGUAGE BangPatterns #-}
import Data.Array
import qualified Data.Map.Lazy as Map
import Data.List
import qualified Data.Set as Set

main :: IO ()
main = do
  (n:d:_) <- fmap (map read . words) getLine :: IO [Integer]
  print $ solveMapDp n d

-- DP using Map
-- Array is not suitable for this problem.
solveMapDp :: Integer -> Integer -> Double
solveMapDp n d = dp Map.! (n, d)
  where
    dp = Map.fromSet f . Set.fromList $ factorCandidates n d
    f :: (Integer, Integer) -> Double
    f (_, 1) = 1
    f (1, 2) = 1 / 2
    f (1, 3) = 1 / 3
    f (1, 4) = 1 / 6
    f (1, 5) = 1 / 6
    f (1, 6) = 1 / 6
    f (1, _) = 0
    f (i, e) = (1/6*) . sum $ map (g i e) [1..6]
      where
        g :: Integer -> Integer -> Integer -> Double
        g j d' k
          | d' `mod` k == 0 =dp Map.! (j - 1, d' `div` k)
          | k `mod` 2 == 0 && d' `mod` 2 == 0 = g j (d' `div` 2) (k `div` 2)
          | k `mod` 3 == 0 && d' `mod` 3 == 0 = g j (d' `div` 3) (k `div` 3)
          | otherwise = dp Map.! (j - 1, d')

factorCandidates :: Integer -> Integer -> [(Integer, Integer)]
factorCandidates n d = do
  ns <- [1..n]
  ms <- factors d
  return (ns, ms)

factors :: (Integral a) => a -> [a]
factors = Map.foldrWithKey (\k v acc -> products (powers k v) acc) [1]
  . toCountMap . primeFactors

powers :: (Integral a) => a -> a -> [a]
powers n m = [ n^i | i <- [0..m]]

products :: (Num a) => [a] -> [a] -> [a]
products xs ys = [ x*y | x <- xs, y <- ys]

toCountMap :: (Ord k, Integral a) => [k] -> Map.Map k a
toCountMap xs = Map.fromListWith (+) $ zip xs (repeat 1)

primeFactors :: (Integral a) => a -> [a]
primeFactors n | n < 2 = []
primeFactors n = go n [2,3,5]
   where
     go !n pps@(p:ps)
       | n < p*p   = [n]
       | r > 0     = go n ps
       | otherwise = p:go q pps
      where
        (q,r) = quotRem n p
     go n [] = [n]



-- memoized DP but RE (maybe heap overflow)
-- This may be because the DP table can be too large.
solveDp :: Integer -> Integer -> Double
solveDp n d = dp ! (n, d)
  where
    dp = listArray ((1,1), (n, d)) $ map f $ candidates n d
    -- f (i, d) はi個のサイコロの目の積がdの倍数になる確率
    f :: (Integer, Integer) -> Double
    f (_, 1) = 1
    f (1, 2) = 1 / 2
    f (1, 3) = 1 / 3
    f (1, 4) = 1 / 6
    f (1, 5) = 1 / 6
    f (1, 6) = 1 / 6
    f (1, _) = 0
    f (i, e) = (1/6*) . sum $ map (g i e) [1..6]
      where
        -- g j d k はj個中1個のサイコロの目がkのとき、それらの目の積がdの倍数になる確率
        -- つまり条件付き確率
        -- g j d k = P [f (j, d) | サイコロのうちひとつの目がk]
        g :: Integer -> Integer -> Integer -> Double
        g j d' k
          | d' `mod` k == 0 = dp ! (j - 1, d' `div` k)
          | k `mod` 2 == 0 && d' `mod` 2 == 0 = g j (d' `div` 2) (k `div` 2)
          | k `mod` 3 == 0 && d' `mod` 3 == 0 = g j (d' `div` 3) (k `div` 3)
          | otherwise = dp ! (j - 1, d')

candidates :: Integer -> Integer -> [(Integer, Integer)]
candidates n d = do
  ns <- [1..n]
  ms <- [1..d]
  return (ns, ms)

solve :: Integer -> Integer -> Double
solve n d = last . map (f d) $ [1..n]
  where
    f 1 1 = 1
    f 2 1 = 1 / 2
    f 3 1 = 1 / 3
    f 4 1 = 1 / 6
    f 5 1 = 1 / 6
    f 6 1 = 1 / 6
    f _ 1 = 0
    f d i = (1/6*) . sum $ map (g d i) [1..6]
      where
        g d i j
          | d `mod` j == 0 = f (d `div` j) (i - 1)
          | j `mod` 2 == 0 && d `mod` 2 == 0 = g (d `div` 2) i (j `div` 2)
          | j `mod` 3 == 0 && d `mod` 3 == 0 = g (d `div` 3) i (j `div` 3)
          | otherwise = f d (i - 1)
