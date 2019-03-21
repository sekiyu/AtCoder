{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
import Data.Array

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve' n as

cost = IntMap.fromList $ zip [1..] [2,5,5,4,5,6,3,7,6]

calc :: [Int] -> Integer
calc as = foldl' (\acc (x, y) -> acc + (fromIntegral x) * y) 0 $ zip (sort as) $ map (10^) [0..]

solve' :: Int -> [Int] -> Integer
solve' n as = calc . snd $ dp!n
  where
    value = IntMap.fromList . filter (\(c, a) -> elem a as) $ zip [2,5,5,4,5,6,3,7,6] [1..] 
    costs = map (\a -> cost IntMap.! a) as
    mincost = minimum costs
    dp = listArray (mincost, n) $ map f [mincost..n] :: Array Int (Int, [Int])
    f :: Int -> (Int, [Int])
    f i | i == mincost = (1, [value IntMap.! mincost])
        | otherwise = select (0, []) $ [ new c | c <- costs]
        where
          new c | i - c >= mincost = (prevl+1, x:prevx)
                | otherwise = (0,[])
                where
                  x = value IntMap.! c
                  (prevl, prevx) = dp!(i - c)
          
          select ret [] = ret
          select (maxl, maxx) (x:xs)
            | maxl < l = select (l, cx) xs
            | maxl > l = select (maxl, maxx) xs
            | maxl == l = if maxx < cx 
                          then select (l, cx) xs
                          else select (maxl, maxx) xs
              where (l, cx) = x 


solve :: Int -> [Int] -> Integer
solve n as = fst $ dp!n
  where
    value = IntMap.fromList . filter (\(c, a) -> elem a as) $ zip [2,5,5,4,5,6,3,7,6] [1..] 
    costs = map (\a -> cost IntMap.! a) as
    mincost = minimum costs
    dp = listArray (mincost, n) $ map f [mincost..n] :: Array Int (Integer, [Int])
    f :: Int -> (Integer, [Int])
    f i | i == mincost = let x = [value IntMap.! mincost] in (calc x, x)
        | otherwise = select (0, []) $ [ new c | c <- costs]
        where
          new c | i - c >= mincost = (calc newx ,newx)
                | otherwise = (0,[])
                where
                  x = value IntMap.! c
                  (_, prevx) = dp!(i - c)
                  newx = x:prevx
          
          select ret [] = ret
          select (maxv, maxx) (x:xs) = let (v, cx) = x in 
                                       if maxv <= v
                                       then select (v, cx) xs
                                       else select (maxv, maxx) xs

