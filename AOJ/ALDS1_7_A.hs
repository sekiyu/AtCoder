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
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents

main :: IO()
main = do
  n <- readLn :: IO Int
  --ts <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  -- print $ head $ solve ts
  ts <- read2dInts
  solve' ts

type AdjList = IntMap.IntMap [Int]
toAdjList :: [[Int]] -> AdjList
toAdjList = foldr f IntMap.empty 
  where
    f :: [Int] -> AdjList -> AdjList
    f (i:n:edges) adjList = IntMap.insert i edges adjList


solve' ts = mapM_ showTree . sortBy (\(a,_,_) (b,_,_) -> compare a b) $ dfs [(root, -1, 0)] IntSet.empty
  where
    adjList = toAdjList ts
    root = findRoot adjList
    findRoot :: AdjList -> Int
    findRoot adjl = head $ [ key | key <- IntMap.keys adjl, key `notElem` (join $ IntMap.elems adjl)]

    dfs :: [(Int, Int, Int)] -> IntSet.IntSet -> [(Int, Int, Int)]
    dfs [] _ = []
    dfs (c:cs) visiteds 
      = c:(dfs (unv++cs) (IntSet.insert current visiteds))
      where
        (current, parent, depth) = c
        unvisiteds = [ x | x <- adjList IntMap.! current, IntSet.notMember x visiteds]
        unv = zip3 unvisiteds (repeat current) (repeat $ depth+1)

    showTree :: (Int, Int, Int) -> IO()
    showTree (c, p, d) = 
      putStrLn $ "node " ++ (show c) ++ ": parent = " ++ (show p) ++ ", depth = " ++ (show d) 
        ++ kind ++ ", [" ++ (intercalate ", " (map show $ adjList IntMap.! c)) ++ "]"
        where
          kind | p == -1 = ", root"
              | null $ adjList IntMap.! c = ", leaf"
              | otherwise = ", internal node"

    {-遅い
    dfs :: [(Int, Int, Int)] -> [Int] -> [(Int, Int, Int)]
    dfs [] _ = []
    dfs (c:cs) visiteds
      | null unvisiteds = c:(dfs cs (current:visiteds))
      | otherwise = dfs ((head unv):c:cs) (current:visiteds)
      where
        (current, parent, depth) = c
        unvisiteds = adjList IntMap.! current \\ visiteds
        unv = zip3 unvisiteds (repeat current) (repeat $ depth+1)
    -}


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


              