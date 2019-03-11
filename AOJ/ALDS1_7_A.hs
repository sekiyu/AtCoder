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
import Data.Array

main :: IO()
main = do
  n <- readLn :: IO Int
  ts <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  -- print $ head $ solve ts
  
  solve' ts
solve :: [[Int]] -> [(Int, Int, [Int])]
solve [] = []
solve (t:ts) = (id, k, cs):solve ts
  where (id:k:cs) = t

data Tree = EmptyTree | Node Int [Tree] deriving (Show)
{-

type Parent = Int
type Left = Int
type Right = Int
data TreePLR = EmptyPLR | Node Int Parent Left Right deriving (Show)
-}


type AdjList = IntMap.IntMap [Int]
toAdjList :: [[Int]] -> AdjList
toAdjList = foldr f IntMap.empty 
  where
    f :: [Int] -> AdjList -> AdjList
    f (i:n:edges) adjList = IntMap.insert i edges adjList


solve' ts = mapM_ showTree . sortOn (\(a,_,_) -> a) $ dfs [(root, -1, 0)] []
  where
    adjList = toAdjList ts
    root = findRoot adjList
    findRoot :: AdjList -> Int
    findRoot adjl = head $ [ key | key <- IntMap.keys adjl, key `notElem` (join $ IntMap.elems adjl)]
    dfs :: [(Int, Int, Int)] -> [Int] -> [(Int, Int, Int)]
    dfs [] _ = []
    dfs (c:cs) visiteds 
      = c:(dfs (unv++cs) (current:visiteds))
      where
        (current, parent, depth) = c
        unvisiteds = [ c | c <- adjList IntMap.! current, c `notElem` visiteds]
        unv = zip3 unvisiteds (repeat current) (repeat $ depth+1)

    showTree :: (Int, Int, Int) -> IO()
    showTree (c, p, d) = 
      putStrLn $ "node " ++ (show c) ++ ": parent = " ++ (show p) ++ ", depth = " ++ (show d) ++ kind ++ ", " ++ (show $ adjList IntMap.! c)
        where
          kind | p == -1 = ", root"
              | null $ adjList IntMap.! c = ", leaf"
              | otherwise = ", internal node"


