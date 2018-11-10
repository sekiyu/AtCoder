{-# LANGUAGE FlexibleContexts #-}
-- ABC006 D - トランプ挿入ソート
import Control.Monad
import Data.Array
import Data.List
import Control.Monad.ST
import Data.Array.ST
import qualified Data.IntMap.Strict as IntMap
import Debug.Trace

main :: IO ()
main = do
  n <- readLn
  cs <- join <$> (replicateM n $ map read . words <$> getLine) :: IO [Int]
  -- print $ solve2 n cs
  print $ partial3 n cs


partial1 :: Int -> [Int] -> Int
partial1 n cs = n - (maximum . map length . filter validate . subsequences $ cs)
  where
    validate c = sort c == c

partial2 :: Int -> [Int] -> Int
partial2 n cs = n - (find [] cs)
  where
    find :: [(Int, Int)] -> [Int] -> Int
    find bs [] = maximum . map snd $ bs
    find [] (c:cs) = find [(c, 1)] cs
    find bs (c:cs) = find ((minMax c bs):bs) cs

    minMax c bs = go 0 c bs
      where
        go m _ [] = (c, m + 1)
        go m c (b:bs)
          | c > fst b = go (max m $ snd b) c bs
          | otherwise = go m c bs


partial3 :: Int -> [Int] -> Int
partial3 n cs = (length cs) - (IntMap.size dp)
  where
    dp = foldl' f IntMap.empty cs
    f :: IntMap.IntMap Int -> Int -> IntMap.IntMap Int
    f dic c
      | null dic = IntMap.insert 1 c dic
      | otherwise = let i = binarySearch c dic 1 (IntMap.size dic)
                    in IntMap.insertWith min (i+1) c dic


-- search an element s.t. minimum [ index of x | x <- somelist, x > t]
-- ss is assumed to be ascending ordered
binarySearch :: Int -> IntMap.IntMap Int -> Int -> Int -> Int
binarySearch t ss left right = if ss IntMap.! right < t
                               then right
                               else if ss IntMap.! left > t
                               then left - 1
                               else innerSearch t ss left right
   where
    innerSearch :: Int -> IntMap.IntMap Int -> Int -> Int -> Int
    innerSearch t ss left right
      | right - left <= 1 = left
      | t < mid = binarySearch t ss left i
      | t > mid = binarySearch t ss i right
      where
        i = div (left + right) 2
        mid = ss IntMap.! i


{-
partial3 :: Int -> [Int] -> Int
partial3 n cs = maximum . elems $ (runSTArray dp)
  where
    dp = newArray (0,n) maxBound :: ST s (STArray s Int Int)
    f 0 table = writeArray table 0 minBound
    f i table = writeArray table imax $ (runSTArray dp)!imax -- need flexiblecontexts
      where
        -- i-1までで最大の要素を探す
        imax = 0

-}


solve2 :: Int -> [Int] -> Int
solve2 n cs = fst $ foldl' f (0, 0) cs
  where
    f :: (Int, Int) -> Int -> (Int, Int)
    f (i, cmax) c
      | c > cmax = (i, c)
      | otherwise = (i+1, cmax)

solve :: Int -> [Int] -> Int
solve n cs = fst $ dp!n
  where
    carray = listArray (1, n) cs
    dp :: Array Int (Int, Int)
    dp = listArray (1, n) $ map f [1..n]
    f 1 = (0, carray!1)
    f i | snd prev < carray!i = (fst prev, carray!i)
        | otherwise = (fst prev + 1, snd prev)
        where
          prev = dp!(i-1)
