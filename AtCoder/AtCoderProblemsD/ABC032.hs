-- ABC032 D - ナップサック問題
import Control.Monad
import Data.List
import qualified Data.IntMap.Strict as M
-- import qualified Data.Map.Strict as M
import Data.Array.Unboxed
-- import Data.Array
import Debug.Trace

main :: IO ()
main = do
  (n:w:_) <- fmap (map read . words) getLine :: IO [Int]
  -- vws <- replicateM (fromIntegral n) $ (\(v:w:_) -> (v, w)) . map (toInteger . read) . words <$> getLine :: IO [(Integer, Integer)]
  vws <- replicateM n $ (\(v:w:_) -> (v, w)) . map read . words <$> getLine :: IO [(Int, Int)]
  let maximumv = maximum . map fst $ vws
      maximumw = maximum . map snd $ vws
      f n mv mw
        | n <= 30 = print $ solveMapDp w vws
        | mw <= 1000 = print $ solveWeightDp w vws
        | mv <= 1000 = print $ solveValueDp w vws
        | otherwise = print $ solveMapDp w vws
  -- f n maximumv maximumw
  print $ solveValueDp w vws


solveValueDp :: Int -> [(Int, Int)] -> Int
solveValueDp maxw vws = maximum . map (snd . fst) . filter (\((i, v), w) -> w <= maxw) $ assocs dp
  where
    n = length vws
    sumv = sum . map fst $ vws
    arrvws = listArray (1, n) vws :: Array Int (Int, Int)
    dp :: Array (Int, Int) Int
    dp = listArray ((0, 0), (n, sumv)) $ map f [(i, j) | i <- [0..n], j <- [0..sumv]]
    f (0, 0) = 0
    f (0, _) = maxBound `div` 2
    f (i, j)
      | j < v = dp!(i-1, j)
      | otherwise = min (dp!(i-1, j)) (dp!(i-1, j - v) + w)
      where
        v = fst $ arrvws!i
        w = snd $ arrvws!i


solveWeightDp :: Int -> [(Int, Int)] -> Int
solveWeightDp maxw vws = dp!(n, maxw)
  where
    n = length vws
    arrvws = listArray (1, n) vws :: Array Int (Int, Int)
    dp :: Array (Int, Int) Int
    dp = listArray ((0, 0), (n, maxw)) $ map f [(i, j) | i <- [0..n], j <- [0..maxw]]
    f (0, _) = 0
    f (_, 0) = 0
    f (i, j)
      | j < w = dp!(i-1, j)
      | otherwise = max (dp!(i-1, j)) (dp!(i-1, j - w) + v)
      where
        v = fst $ arrvws!i
        w = snd $ arrvws!i

-- Solution using IntMap
solveMapDp :: Int -> [(Int, Int)] -> Int
solveMapDp maxw vws = maximum . M.elems $ dp
  where
    initial = M.singleton 0 0
    dp = foldr f initial vws
    -- dp = foldl' (flip f) initial vws
    f :: (Int, Int) -> M.IntMap Int -> M.IntMap Int
    f (v, w) table = foldr g table $ M.keys table
      where
        g :: Int -> M.IntMap Int -> M.IntMap Int
        g i table
          | i + w > maxw = table
          | otherwise = M.insertWith max (i+w) ((table M.! i) + v) table

-- Definitive solution
solve :: Int -> [(Int, Int)] -> Int
solve w vws = maximum . map (sum . map fst)
  . filter (\c -> w >= (sum . map snd $ c)) $ subsequences vws
