-- ABC111 C - /\/\/\/
import Control.Monad
import Data.List
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import Debug.Trace

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  -- print $ solve ss
  print $ solve' ss

solve' :: [Int] -> Int
solve' ss
  | IntSet.size uniques == 1 = n
  | otherwise = go ss (IntMap.empty, IntMap.empty)
  where
    uniques = IntSet.fromList ss
    n = (length ss) `div` 2
    go :: [Int] -> (IntMap.IntMap Int, IntMap.IntMap Int) -> Int
    go [] (odds, evens) = if okey == ekey
                          then 2 * n - max (oval + e2) (eval + o2)
                          else 2 * n - oval - eval
      where
        (okey, oval) = lookupMax odds
        (ekey, eval) = lookupMax evens
        o2 = if (IntMap.size odds) > 1
             then (sort . IntMap.elems $ odds) !! (length odds - 2)
             else minBound
        e2 = if (IntMap.size evens) > 1
             then (sort . IntMap.elems $ evens) !! (length evens - 2)
             else minBound
    go (a:b:as) (odds, evens) = go as
      (IntMap.insertWith (+) a 1 odds, IntMap.insertWith (+) b 1 evens)

-- IntMap.lookupMaxがAtCoderになかったので自分で実装
lookupMax :: IntMap.IntMap Int -> (Int, Int)
lookupMax = last . sortOn snd . IntMap.toList

-- 全探索　遅い
solve :: [Int] -> Int
solve ss
  | IntSet.size uniques == 1 = length ss `div` 2
  | otherwise = minimum $ map (countNotEqual ss) $ candidates uniques
  where
    uniques = IntSet.fromList ss

candidates :: IntSet.IntSet -> [[Int]]
candidates us = map toInf pairs
  where
    pairs = filter (\(a:b:_) -> not (a == b)) $ replicateM 2 $ IntSet.toList us
    toInf :: [Int] -> [Int]
    toInf pair = pair ++ (toInf pair)

countNotEqual :: [Int] -> [Int] -> Int
countNotEqual _ [] = 0
countNotEqual [] _ = 0
countNotEqual (a:as) (b:bs) = (if a == b then 0 else 1) + (countNotEqual as bs)
