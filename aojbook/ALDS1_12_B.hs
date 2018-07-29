import           Control.Monad
import           Data.Functor
import           Data.List
import qualified Data.Map.Strict as Map
import           Debug.Trace

--main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO([[Int]])
  --print $ toAdjList am
  forM_ (Map.toList . solve . toAdjList $ am) (\(node, distance) -> putStrLn $ show node ++ " " ++ show distance)

type Node = Int
type Weight = Int
type AdjList = [Map.Map Node Weight]
toAdjList :: [[Int]] -> [Map.Map Node Weight]
toAdjList [] = []
toAdjList (a:as) = (adjacents a):(toAdjList as)
  where
    adjacents :: [Int] -> Map.Map Node Weight
    adjacents (i:n:bs) = Map.fromList $ toPair bs
    toPair :: [Int] -> [(Node, Weight)]
    toPair [] = []
    toPair (x:y:zs) = (x, y):toPair zs


solve :: AdjList -> Map.Map Node Int
--solve _ = Map.fromList [(1,2), (3,4)]
solve al = dijkstra al $ Map.singleton 0 0
  where
    dijkstra :: AdjList -> Map.Map Node Int -> Map.Map Node Int
    dijkstra al ss
      | length al <= Map.size ss = ss
      | otherwise = dijkstra al $ Map.insert i d ss
      -- | otherwise = trace ("adj = " ++ show adjacents) dijkstra al $ Map.insert i d ss
      where
        adjacents = join [[(j, ss Map.! i + (al !! i) Map.! j) | j <- Map.keys (al !! i), j `notElem` Map.keys ss]
            | i <- [0..(length al - 1)], i `elem` Map.keys ss]
        (i, d) = minimumBy (\x y -> compare (snd x) (snd y)) adjacents
