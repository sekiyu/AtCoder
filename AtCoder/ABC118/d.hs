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
  print $ solve n as

cost = IntMap.fromList $ zip [1..] [2,5,5,4,5,6,3,7,6]

calc as = foldl' (\acc (x, y) -> acc + (fromIntegral x) * y) 0 $ zip (sort as) $ map (10^) [0..]

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

