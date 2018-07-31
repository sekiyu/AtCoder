{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Data.Functor
import           Data.List
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO([[Int]])
  forM_ (Map.toList . solve . toAdjList $ am)
    (\(node, distance) -> putStrLn . unwords . map show $ [node, distance])

type Node = Int
type Weight = Int
type AdjList = [Map.Map Node Weight]

toAdjList :: [[Int]] -> AdjList
toAdjList [] = []
toAdjList ( a:as) = (adjacents a):(toAdjList as)
  where
    adjacents :: [Int] -> Map.Map Node Weight
    adjacents (i:n:bs) = Map.fromList $ toPair bs
    toPair :: [Int] -> [(Node, Weight)]
    toPair [] = []
    toPair (x:y:zs) = (x, y):toPair zs

solve :: AdjList -> Map.Map Node Int
solve !al = dijkstra al $ Map.singleton 0 0
  where
    dijkstra :: AdjList -> Map.Map Node Int -> Map.Map Node Int
    dijkstra al ss
      | length al <= Map.size ss = ss
      | otherwise = dijkstra al $ Map.insert i d ss
      -- | otherwise = trace ("adj = " ++ show adjacents) dijkstra al $ Map.insert i d ss
      where
        adjacents = foldr newVal None $ join [[(j, ss Map.! i + (al !! i) Map.! j) | j <- Map.keys (al !! i), j `notElem` Map.keys ss] | i <- [0..(length al - 1)], i `elem` Map.keys ss]
        (i, d) = getMin adjacents

data MinHolder a = None | MinHolder a deriving Show
newVal :: (Ord a) => (b, a) -> MinHolder (b, a) -> MinHolder (b, a)
newVal a None = MinHolder a
newVal (a, b) (MinHolder (!x, !y)) =
  if b > y
    then MinHolder (x, y)
    else MinHolder (a, b)
getMin :: MinHolder a -> a
getMin (MinHolder m) = m
