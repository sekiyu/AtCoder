import Data.List
import Control.Monad
import qualified Data.Map.Strict as Map

-- 並べ換え n!通り
-- Prelude Data.List> permutations [1..3]
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]

-- 重複あり順列 m^n通り
-- Prelude Control.Monad> replicateM 2 "abcde"
-- ["aa","ab","ac","ad","ae","ba","bb","bc","bd","be","ca","cb","cc","cd","ce","da","db","dc","dd","de","ea","eb","ec","ed","ee"]

-- 部分リスト 2^n通り
-- Prelude Data.List > subsequences ['d'..'f']
-- ["","d","e","de","f","df","ef","def"]
-- 以下も同じこと
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- 連続部分列
continuousSubsequences :: [a] -> [[a]]
continuousSubsequences xs = do
  let n = length xs
  s <- [0..(n-1)]
  c <- [1..(n-s)]
  return . take c . drop s $ xs

-- continuousSubsequences xs = [ (take j . drop i $ xs) | i <- [0..(n-1)], j <- [1..(n-i)]]
--   where n = length xs
  
{- 関数の合成の基本
  Prelude> let f = foldr (.) id [(+8), (*100), (+1)]
  Prelude> f 1
  208
-}
-- リストを返す関数を合成して不確定計算
inMany :: (Monad m) => (x -> m x) -> Int -> x -> m x
inMany f n x = return x >>= foldr (<=<) return (replicate n f)

-- 前に1 or 後ろに1 進む虫が10から始めて4単位時間後の位置
-- *Main> let move = (\x -> [x+1, x-1])
-- *Main> inMany move 4 10
-- [14,12,12,10,12,10,10,8,12,10,10,8,10,8,8,6]

{- モナド基本
  Prelude> let twice = (\x -> [x, x^2])
  Prelude> twice 3
  [3,9]
  Prelude> return 3 >>= twice
  [3,9]
  Prelude> return 3 >>= twice >>= twice
  [3,9,9,81]
-}


-- リスト要素の数え上げ
toCountMap :: (Ord k) => [k] -> Map.Map k Int
toCountMap xs = Map.fromListWith (+) $ zip xs (repeat 1)
-- toCountMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]

-- Combination
comb :: Int -> Int -> [(Int, Int)]
comb n m = do
  ns <- [1..n]
  ms <- [1..m]
  return (ns, ms)
-- *Main> comb 2 3
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]

-- Applicative style
comb2 n m = (,) <$> [1..n] <*> [1..m]


pascalTriangle n = do
  ns <- [1..n]
  ms <- [1..ns]
  return (ns, ms)
-- *Main> pascalTriangle 4
-- [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)]
