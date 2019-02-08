import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
import Data.Array

main :: IO ()
main = do
  n <- readLn
  gs <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  -- putStrLn . unwords . map show $ solve gs
  print $ solve gs

-- solve :: [[Int]] -> [Int]
solve = reverse . dfs [1] [] . toAdjList

-- dfs :: stackToVisit -> visiteds -> AdjcentList -> visiteds
dfs :: [Int] -> [Int] -> AdjList -> [Int]
dfs [] visiteds _ = visiteds
dfs (s:ss) visiteds adjList
  | null unvisiteds = dfs ss (s:visiteds) adjList
  | otherwise = dfs ((head unvisiteds):s:ss) (s:visiteds) adjList
    where
      edges = adjList IntMap.! s
      unvisiteds = filter (\i -> not(i `elem` visiteds)) edges


type AdjList = IntMap.IntMap [Int]

toAdjList :: [[Int]] -> AdjList
toAdjList = foldr f IntMap.empty 
  where
    f :: [Int] -> AdjList -> AdjList
    f (i:n:edges) adjList = IntMap.insert i (sort edges) adjList