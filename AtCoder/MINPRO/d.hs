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
-- import Data.Array
import Data.Array.Unboxed

main :: IO ()
main = do
  l <- readLn
  as <- replicateM l $ readLn :: IO [Int]
  -- print $ solve l as
  print $ solve' as

solve :: Int -> [Int] -> Int
solve l as = minimum $ map (\i -> dp!(l, i)) [0..4]
  where
    arr = listArray (1, l) as :: UArray Int Int
    dp :: Array (Int, Int) Int
    dp = listArray ((0, 0), (l, 4)) $ map f [ (x,b) | x <- [0..l], b <- [0..4]]
    f :: (Int, Int) -> Int
    f (0, _) = 0
    f (i, choice) = case choice of
      0 -> leftIgnore + arr!i
      1 -> (minimum . take 2 $ choices) + if arr!i == 0 then 2 else if arr!i `mod` 2 == 0 then 0 else 1
      2 -> (minimum . take 3 $ choices) + if arr!i `mod` 2 == 0 then 1 else 0
      3 -> (minimum . take 4 $ choices) + if arr!i == 0 then 2 else if arr!i `mod` 2 == 0 then 0 else 1
      4 -> (minimum choices) + arr!i
      where
        leftIgnore = dp!(i-1, 0)
        leftFlap = dp!(i-1, 1)
        oneWay = dp!(i-1, 2)
        rightFlap = dp!(i-1, 3)
        rightIgnore = dp!(i-1, 4)
        choices = [leftIgnore, leftFlap, oneWay, rightFlap, rightIgnore]

solve' :: [Int] -> Int
solve' = minimum . foldl' f (replicate 5 0) 
  where
    f :: [Int] -> Int -> [Int]
    f !as !a = [
      (head as) + a,
      (minimum . take 2 $ as) + if a == 0 then 2 else if a `mod` 2 == 0 then 0 else 1,
      (minimum . take 3 $ as) + if a `mod` 2 == 0 then 1 else 0,
      (minimum . take 4 $ as) + if a == 0 then 2 else if a `mod` 2 == 0 then 0 else 1,
      (minimum as) + a]

{-
    f (i, 0) | arr!i == 0 = 2 + min (dp!(i-1, 0)) (dp!(i-1, 2))
             | arr!i `mod` 2 == 0 = min (dp!(i-1, 0)) (dp!(i-1, 2))
             | otherwise = min (dp!(i-1, 0) + 1) (dp!(i-1, 2))
    f (i, 1)  = minimum [dp!(i-1, 1), db, non] + if arr!i `mod` 2 == 0 then 1 else 0
              where 
                db = dp!(i-1, 0)
                non = dp!(i-1, 2)
    f (i, 2) = dp!(i-1, 2) + arr!i
    f (i, 3) = s + minimum [dp!(i-1, 0), dp!(i-1, 1), dp!(i-1, 2)]
      where s = sum $ drop (i-1) as
-}
              {-
solve' l as = min (dp!(l, True)) (dp!(l, False))
  where
    arr = listArray (1, l) as
    dp = listArray ((1, False), (l, True)) $ map f [ (x,b) | x <- [1..l], b <- [False, True]]
    f (1, True) = if (arr!1) `mod` 2 == 0
                  then 0
                  else 1
    f (1, False) = if (arr!1) `mod` 2 == 0
                   then 1
                   else 0
    f (i, True) | i == 0 = dp!(i-1, True) + 2
                | i `mod` 2 == 0 = dp!(i-1, True)
                | otherwise = dp!(i-1, True) + 1
    f (i, False) | i == 0 = min (dp!(i, True)) (dp!(i-1, False) + 1)
                 | i `mod` 2 == 0 = dp!(i-1, False) + 1
                 | otherwise = dp!(i-1, False)
-}

