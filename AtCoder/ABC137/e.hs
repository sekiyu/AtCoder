{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad hiding (join)
import Data.Maybe
import Debug.Trace
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
-- import Data.Array
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST


type Edge = (Int, Int, Int)
type Edges = Set.Set Edge

from :: Edge -> Int
from (f, _, _) = f

to :: Edge -> Int
to (_, t, _) = t

weight :: Edge -> Int
weight (_, _, w) = w

-- weightedEdge :: Int -> Int -> Int -> WeightedEdge
-- weightedEdge f t w = ((f, t), w)

data Graph = Graph Int (Map.Map Int Edges)
  deriving (Show)

empty :: Graph
empty = Graph 0 Map.empty

addEdge :: Edge -> Graph -> Graph
addEdge edge (Graph v mp) = Graph (v + 1) $ 
  Map.insertWith Set.union (from edge) (Set.singleton edge) mp

adj :: Int -> Graph -> Edges
adj i (Graph _ mp) = mp Map.! i

edges :: Graph -> Edges
edges (Graph _ mp)= Set.unions . Map.elems $ mp

-- number of vertices
vertices :: Graph -> Int
vertices (Graph vv _) = vv

-- fromInput :: [[from, to, weight]] -> Graph
fromInput :: [[Int]] -> Graph
fromInput = foldl' (\graph (a:b:c:_) -> addEdge (a, b, c) graph) empty 

-- bellmanFord :: start -> Graph -> ShortestPaths or Nothing(Negative cycle exists)
-- bellmanFord :: Int -> Graph -> UArray Int Int
-- bellmanFord start graph = runSTUArray $ do
--   let v = vertices graph
--   dist <- newArray (1, v) (maxBound :: Int)
--   writeArray dist start 0
--   forM_ [1..v] $ \_ -> do
--     loopEdges (edges graph) dist
--   return dist


bellmanFord :: Int -> Graph -> UArray Int Int -> UArray Int Int
bellmanFord start graph initial = runSTUArray $ do
  let v = vertices graph
  -- dist <- newArray (1, v) (maxBound :: Int)
  dist <- thaw initial
  writeArray dist start 0
  forM_ [1..v] $ \_ -> do
    loopEdges (edges graph) dist
  return dist


loopEdges :: (Foldable t, MArray a Int m) => t Edge -> a Int Int -> m (a Int Int)
loopEdges es dist = do
  forM_ es $ \e -> do
    relaxEdge e dist
  return dist

-- loopEdges' :: (IArray a1 Int, MArray a2 Int m, Foldable t) 
--   => t Edge -> a1 Int Int -> m (a2 Int Int)
-- loopEdges' es dist = do
--   prev <- thaw dist
--   forM_ es $ \e -> do
--     relaxEdge e prev
--   return prev
  

-- relaxEdge :: Edge -> UArray Int Int -> ST s (STUArray t Int Int)
relaxEdge :: MArray a Int m => Edge -> a Int Int -> m (a Int Int)
relaxEdge e dist = do
  f <- readArray dist $ from e
  t <- readArray dist $ to e
  if f == maxBound
    then return ()
    else writeArray dist (to e) $ min (f + weight e) t
  return dist

sampleGraph = t
  where
    f = addEdge (1, 2, 1) empty
    s = addEdge (3, 2, 0) f
    t = addEdge (1, 3, 2) s

-- ABC137 E - Coins Respawn
main :: IO ()
main = do
  (n:m:p:_) <- map read . words <$> getLine :: IO [Int]
  abcs <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  print $ solve n m p abcs
    
solve n m p abcs = if hasNegativeCycle then -1 else  max 0 . negate . (!n) $ dist
  where
    graph = fromInput . map (\[a,b,c] -> [a,b,p-c] ) $ abcs
    dist = bellmanFord 1 graph $ listArray (1, n) (replicate n maxBound) :: UArray Int Int
    test = bellmanFord 1 graph dist
    hasNegativeCycle = dist!n /= test!n
    
