import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap




---------- グラフの隣接リストへの変換
type AdjList = IntMap.IntMap [Int]

-- GRL_4_Bのような形式でグラフが与えられる場合
-- sts <- replicateM e $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
-- と組み合わせる
toAdjList :: [(Int, Int)] -> AdjList
toAdjList = foldr insert IntMap.empty 
  where
    insert :: (Int, Int) -> AdjList -> AdjList
    insert (a, b) adjList = IntMap.insertWith (++) a [b] adjList

-- ALDS1_11_Bのような形式でグラフが与えられる場合
toAdjList :: [[Int]] -> AdjList
toAdjList = foldr f IntMap.empty 
  where
    f :: [Int] -> AdjList -> AdjList
    f (i:n:edges) adjList = IntMap.insert i edges adjList
    
------------ 隣接リストを使うDFS
-- dfs :: stackToVisit -> visiteds -> AdjcentList -> visiteds
dfs :: [Int] -> [Int] -> AdjList -> [Int]
dfs [] visiteds _ = visiteds
dfs (s:ss) visiteds adjList
  | null unvisiteds = dfs ss (s:visiteds) adjList
  | otherwise = dfs ((head unvisiteds):s:ss) (s:visiteds) adjList
    where
      edges = adjList IntMap.! s
      unvisiteds = filter (\i -> not(i `elem` visiteds)) edges
