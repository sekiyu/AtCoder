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
-- import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST

main :: IO ()
main = do
  n <- readLn :: IO Int
  uvws <- replicateM (n-1) $ map read . words <$> getLine :: IO [[Int]]
  putStr . unlines . map show $ solve' n uvws

solve' :: Int -> [[Int]] -> [Int]
solve' n uvws = map color [1..n]
  where
    (evens, odds) = partition (\(_:_:w:_) -> even w) uvws
    initial = ufFromList [0..n]
    evenUf = foldl' (\tree (u:v:w:_) -> ufUnite u v tree) initial $ evens
    zero = if null evens 
           then 1
           else (\(u:_:_:_) -> u) $ head evens
    zeroRoot = ufFind zero evenUf
    
    uf = go evenUf odds
    go tree [] = tree
    go tree ((u:v:_:_):uvws) 
      | ufFind u tree == zeroRoot = go (ufUnite v 0 tree) uvws
      | ufFind v tree == zeroRoot = go (ufUnite u 0 tree) uvws
      | otherwise = go tree $ uvws ++ [[u, v, 0]]
    color i = if ufFind i uf == zeroRoot
      then 0
      else 1

type UnionFind = (IntMap.IntMap Int, IntMap.IntMap Int)

ufFromList :: [Int] -> UnionFind
ufFromList as = (IntMap.fromList $ zip as as, IntMap.fromList $ zip as (repeat 1))

ufIsRoot :: Int -> UnionFind -> Bool
ufIsRoot k uf = k == ((fst uf) IntMap.! k)

ufFind :: Int -> UnionFind -> Int 
ufFind k uf | ufIsRoot k uf = k
            | otherwise = ufFind ((fst uf) IntMap.! k) uf

ufSize :: Int -> UnionFind -> Int
ufSize k uf = let p = ufFind k uf in (snd uf) IntMap.! p

ufUnite :: Int -> Int -> UnionFind -> UnionFind
ufUnite k l uf 
  | (ufFind k uf) == (ufFind l uf) = uf
  | otherwise = if nk < nl 
                then (IntMap.adjust (\_->pl) pk $ fst uf, IntMap.adjust (\_->nk+nl) pl $ snd uf)
                else (IntMap.adjust (\_->pk) pl $ fst uf, IntMap.adjust (\_->nk+nl) pk $ snd uf)
    where
      pk = ufFind k uf
      nk = ufSize k uf
      pl = ufFind l uf
      nl = ufSize l uf          

ufIsSameGroup :: Int -> Int -> UnionFind -> Bool
ufIsSameGroup i j uf = ufFind j uf == ufFind j uf

-- solve :: Int -> [[Int]] -> [Int]
-- solve n uvws = elems colors
--   -- map fst $ Map.elems dic
  
--   where
--     dic = foldl' addDic Map.empty uvws
--     addDic d (u:v:w:_) = let tmp = Map.insert u (v, w) d
--                          in Map.insert v (u, w) tmp


--     colors :: Array Int Int
--     colors = listArray (1, n) $ map f [1..n]
--     f 1 = 0
--     f i = let (v, w) = dic Map.! i
--           in if even w 
--              then colors!v 
--              else (colors!v + 1) `mod` 2

