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
import qualified Data.ByteString.Char8 as B
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents :: IO [[Int]]


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

empty :: Int -> Graph
empty v = Graph v Map.empty

addEdge :: Edge -> Graph -> Graph
addEdge edge (Graph v mp) = Graph v $ 
  Map.insertWith Set.union (from edge) (Set.singleton edge) mp

adj :: Int -> Graph -> Edges
adj i (Graph _ mp) = mp Map.! i

edges :: Graph -> Edges
edges (Graph _ mp)= Set.unions . Map.elems $ mp

-- number of vertices
vertices :: Graph -> Int
vertices (Graph vv _) = vv

-- fromInput :: [[from, to, weight]] -> Graph
fromInput :: Int -> [[Int]] -> Graph
fromInput v = foldl' (\graph (a:b:c:_) -> addEdge (a, b, c) graph) (empty v)

bellmanFord :: Int -> Graph -> UArray Int Int
bellmanFord start graph = runSTUArray $ do
  let v = vertices graph
  dist <- newArray (1, v) (inf :: Int)
  -- dist <- thaw initial
  writeArray dist start 0
  forM_ [1..v] $ \_ -> do
    forM_ (edges graph) (flip relaxEdge dist)
  return dist

-- relaxEdge :: Edge -> UArray Int Int -> ST s (STUArray t Int Int)
relaxEdge :: MArray a Int m => Edge -> a Int Int -> m (a Int Int)
relaxEdge e dist = do
  f <- readArray dist $ from e
  t <- readArray dist $ to e
  if f == inf
    then return ()
    else writeArray dist (to e) $ min (f + weight e) t
  return dist

inf :: Int
inf = 10^10

detect :: Int -> Graph -> UArray Int Int -> UArray Int Int
detect start graph initial = runSTUArray $ do
  let v = vertices graph
  -- dist <- newArray (1, v) (maxBound :: Int)
  dist <- thaw initial
  forM_ [1..v] $ \_ -> do
    forM_ (edges graph) $ \e -> do
        f <- readArray dist $ from e
        t <- readArray dist $ to e
        if (f /= inf) && (f + weight e < t)
          then writeArray dist (to e) $ negate inf
          else return () 
        return dist
  return dist


-- ABC137 E - Coins Respawn
main :: IO ()
main = do
  (n:m:p:_) <- map read . words <$> getLine :: IO [Int]
  abcs <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  -- abcs <- read2dInts
  print $ solve n m p abcs
    
solve n m p abcs = if hasEffectiveNegativeCycle then -1 else  max 0 . negate . (!n) $ dist
  where
    graph = fromInput n . map (\[a,b,c] -> [a,b,p-c] ) $ abcs
    dist = bellmanFord 1 graph :: UArray Int Int
    test = detect 1 graph dist
    hasEffectiveNegativeCycle = test!n <= negate inf

    -- test = bellmanFord 1 graph dist
    -- hasNegativeCycle = dist!n /= test!n
    
