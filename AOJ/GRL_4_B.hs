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

main :: IO ()
main = do
  (v:e:_) <- fmap (map read . words) getLine :: IO [Int]
  sts <- replicateM e $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  putStrLn . unlines . map show $ solve'' v sts

solve'' v = reverse . dfs' [0..(v-1)] [] IntSet.empty . toAdjList

type AdjList = IntMap.IntMap [Int]

-- ちょっとだけ高速化
dfs' :: [Int] -> [Int] -> IntSet.IntSet -> AdjList -> [Int]
dfs' [] visiteds _ _ = visiteds
dfs' (s:ss) visiteds vset adjList
  | IntSet.member s vset = dfs' ss visiteds vset adjList -- 訪問済のノード
  | IntMap.notMember s adjList = dfs' ss (s:visiteds) newset adjList -- 入次数0のノード
  | null unvisiteds = dfs' ss (s:visiteds) newset adjList -- 入字数0ではないが、上流が全て探索済みのノード
  | otherwise = dfs' ((head unvisiteds):s:ss) visiteds vset adjList
    where
      unvisiteds = (adjList IntMap.! s) \\ visiteds
      newset = IntSet.insert s vset


solve' v = reverse . dfs [0..(v-1)] [] . toAdjList

dfs :: [Int] -> [Int] -> AdjList -> [Int]
dfs [] visiteds _ = visiteds
dfs (s:ss) visiteds adjList
  | elem s visiteds = dfs ss visiteds adjList -- 訪問済のノード
  | IntMap.notMember s adjList = dfs ss (s:visiteds) adjList -- 入次数0のノード
  | null unvisiteds = dfs ss (s:visiteds) adjList -- 入字数0ではないが、上流が全て探索済みのノード
  | otherwise = dfs ((head unvisiteds):s:ss) visiteds adjList
    where
      unvisiteds = (adjList IntMap.! s) \\ visiteds

-- 入次数が0のノードを探したいので向きを逆にする
toAdjList :: [(Int, Int)] -> AdjList
toAdjList = foldr insert IntMap.empty 
  where
    insert :: (Int, Int) -> AdjList -> AdjList
    insert (a, b) adjList = IntMap.insertWith (++) b [a] adjList
  

-- solve :: Int -> [(Int, Int)] -> [Int]
solve v sts = traceShow adjList $ f adjList IntSet.empty $ head (IntMap.keys adjList)
  where
    adjList = toAdjList sts
    f :: AdjList -> IntSet.IntSet -> Int -> [Int]
    f al visited visiting
      | IntSet.member visiting visited = []
      | IntMap.member visiting al = foldr (++) [visiting] $ map (f al (IntSet.insert visiting visited)) (al IntMap.! visiting)
      | otherwise = [visiting]
    
