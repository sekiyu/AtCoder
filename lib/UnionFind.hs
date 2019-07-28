import qualified Data.Map.Strict as Map
-- import qualified Data.IntMap.Strict as IntMap


-- Disjoint Set
type UnionFind a = Map.Map a a

ufFromList :: (Ord a) => [a] -> UnionFind a
ufFromList as = Map.fromList $ zip as as

ufIsRoot :: (Ord a) => a -> UnionFind a -> Bool
ufIsRoot k uf = k == (uf Map.! k)

ufFind :: (Ord a) => a -> UnionFind a -> a 
ufFind k uf | ufIsRoot k uf = k
            | otherwise = ufFind (uf Map.! k) uf

-- slow
ufSize :: (Ord a) => a -> UnionFind a -> Int
ufSize k uf = let parent = ufFind k uf
              in Map.foldr (\p acc -> acc + (if p == parent then 1 else 0 )) 0 uf

ufUnite :: (Ord a) => a -> a -> UnionFind a -> UnionFind a
ufUnite k l uf 
  | pk == pl = uf
  | otherwise = Map.map (\p -> if p == pl then pk else p) uf
    where
      pk = ufFind k uf
      pl = ufFind l uf

ufIsSameGroup :: (Ord a) =>  a -> a -> UnionFind a -> Bool
ufIsSameGroup i j uf = ufFind j uf == ufFind j uf
