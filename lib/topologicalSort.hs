import Data.List
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

type AdjList = IntMap.IntMap [Int]

topologocalSort :: AdjList -> [Int]
topologocalSort as = reverse . dfs nodes [] IntSet.empty $ reverseDirection as
  where
    nodes = nub $ (IntMap.keys as) ++ join (IntMap.elems as)

dfs :: [Int] -> [Int] -> IntSet.IntSet -> AdjList -> [Int]
dfs [] visiteds _ _ = visiteds
dfs (s:ss) visiteds vset adjList
  | IntSet.member s vset = dfs ss visiteds vset adjList -- 訪問済のノード
  | IntMap.notMember s adjList = dfs ss (s:visiteds) newset adjList -- 入次数0のノード
  | null unvisiteds = dfs ss (s:visiteds) newset adjList -- 入字数0ではないが、上流が全て探索済みのノード
  | otherwise = dfs ((head unvisiteds):s:ss) visiteds vset adjList
    where
      unvisiteds = (adjList IntMap.! s) \\ visiteds
      newset = IntSet.insert s vset

-- Reverse direction of directed graph
reverseDirection :: AdjList -> AdjList
reverseDirection = IntMap.foldrWithKey f IntMap.empty
  where
    f :: Int -> [Int] -> AdjList -> AdjList
    f s gs adjList = foldr (\g al-> IntMap.insertWith (++) g [s] al) adjList gs 
