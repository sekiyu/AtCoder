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

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  abs <- replicateM m $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  putStr . unlines . map show $ solve' n m abs
  -- solve n m abs
  -- return ()

solve' n m abs = tail . reverse . scanl' (-) (pairs n) $ unfoldr f (initial, reverse abs)
  where
    initial = ufFromList [1..n]
    -- f :: (UnionFind Int, [(Int, Int)]) -> Maybe (Int, (UnionFind Int, [(Int, Int)]))
    f (_, []) = Nothing
    f (uf, !(!a,!b):abs) = Just (if pa == pb then 0 else na * nb, (newUf, abs))
      where
        pa = ufFind a uf
        na = ufSize a uf
        pb = ufFind b uf
        nb = ufSize b uf
        newUf = ufUnite a b uf

pairs n = n * (n - 1) `div` 2

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


{-
type UnionFind a = Map.Map a (a, Int)

ufFromList :: (Ord a) => [a] -> UnionFind a
ufFromList as = Map.fromList $ zip as $ zip as (repeat 1)

ufIsRoot :: (Ord a) => a -> UnionFind a -> Bool
ufIsRoot k uf = k == (fst $ uf Map.! k)

ufFindImpl :: (Ord a) => a -> UnionFind a -> (a, Int)
ufFindImpl k uf | ufIsRoot k uf = uf Map.! k
                | otherwise = ufFindImpl (fst $ uf Map.! k) uf

ufFind :: (Ord a) => a -> UnionFind a -> a
ufFind k = fst . ufFindImpl k 

ufSize :: (Ord a) => a -> UnionFind a -> Int
ufSize k = snd . ufFindImpl k

ufUnite :: (Ord a) => a -> a -> UnionFind a -> UnionFind a
ufUnite k l uf 
  | (ufFind k uf) == (ufFind l uf) = uf
  | otherwise = if nk < nl 
                then Map.adjust (\_ -> (pl, nk + nl)) pl $ Map.adjust (\_ -> (pl, 0)) pk uf
                else Map.adjust (\_ -> (pk, nk + nl)) pk $ Map.adjust (\_ -> (pk, 0)) pl uf
    where
      (pk, nk) = ufFindImpl k uf
      (pl, nl) = ufFindImpl l uf
-}

{-
solve n m abs = foldM (\uf (a, b) -> do
  let na = ufSize a uf
      nb = ufSize b uf
  print $ negate (na * nb)
  return $ ufUnite a b uf
  ) initial abs
  where
    initial = ufFromList [1..n]
    fs = map (uncurry ufUnite) (reverse abs)
    -- foldr ($) initial fs
    
-}