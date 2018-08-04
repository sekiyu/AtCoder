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
    (\(!node, !distance) -> putStrLn . unwords . map show $ [node, distance])

type Node = Int
type Weight = Int
type AdjList = [Map.Map Node Weight]

toAdjList :: [[Int]] -> AdjList
toAdjList [] = []
toAdjList (a:as) = (adjacents a):(toAdjList as)
  where
    adjacents :: [Int] -> Map.Map Node Weight
    adjacents (i:n:bs) = Map.fromList $ toPair bs
    toPair :: [Int] -> [(Node, Weight)]
    toPair [] = []
    toPair (x:y:zs) = (x, y):toPair zs

solve :: AdjList -> Map.Map Node Int
-- solve !al = dijkstra al $ Map.singleton 0 0
solve !al = dijkstra' al initialAdjs initialVisits
  where
    dijkstra :: AdjList -> Map.Map Node Int -> Map.Map Node Int
    dijkstra al ss
      | length al <= Map.size ss = ss
      | otherwise = dijkstra al $ Map.insert i d ss
      -- | otherwise = trace ("adj = " ++ show adjacents) dijkstra al $ Map.insert i d ss
      where
        adjacents = foldr newVal None $ join [[(j, ss Map.! i + (al !! i) Map.! j) | j <- Map.keys (al !! i), j `notElem` Map.keys ss] | i <- [0..(length al - 1)], i `elem` Map.keys ss]
        (i, d) = getMin adjacents

    initialVisits = Map.singleton 0 0
    initialAdjs = findAdjs 0 al Empty initialVisits

    dijkstra' :: AdjList -> MinHeap (Node, Int) -> Map.Map Node Int -> Map.Map Node Int
    dijkstra' !al !adjs !visits
      | length al <= Map.size visits = visits
      | otherwise = dijkstra' al nextAdjs newVisits
      where
        ((i, d), newAdjs) = visit adjs visits
        newVisits = Map.insert i d visits
        nextAdjs = findAdjs i al newAdjs newVisits

        visit :: (Ord a, Eq b) => MinHeap (b, a) -> Map.Map b a -> ((b, a), MinHeap (b, a))
        visit adjs visits = if i `elem` Map.keys visits
                            then visit newAdjs visits
                            else ((i, d), newAdjs)
          where
            ((i, d), newAdjs) = extract adjs

    findAdjs ::  Int -> AdjList -> MinHeap (Node, Int) -> Map.Map Node Int -> MinHeap (Node, Int)
    findAdjs i al adjs visits
      = foldr insertMH adjs [(j, visits Map.! i + (al !! i) Map.! j) | j <- Map.keys (al !! i), j `notElem` Map.keys visits]

        -- ((i, d), newadjs) = extract . foldr insertMH adjs
          -- $ join [[(j, visits Map.! i + (al !! i) Map.! j) | j <- Map.keys (al !! i), j `notElem` Map.keys visits] | i <- [0..(length al - 1)], i `elem` Map.keys visits]


data MinHeap a = Empty | Node a (MinHeap a) (MinHeap a) deriving Show
insertMH :: (Ord a) => (b, a) -> MinHeap (b, a) -> MinHeap (b, a)
insertMH a Empty = Node a Empty Empty
insertMH a t = merge (Node a Empty Empty) t
extract ::(Ord a) => MinHeap (b, a) -> ((b, a), MinHeap (b, a))
extract (Node x left right) = (x, merge left right)
merge :: (Ord a) => MinHeap (b, a) -> MinHeap (b, a) -> MinHeap (b, a)
merge t Empty = t
merge Empty t = t
merge a@(Node (ia, vala) al ar) b@(Node (ib, valb) bl br)
  | vala < valb     = Node (ia, vala) ar (merge al b)
  | otherwise       = Node (ib, valb) br (merge bl a)

data MinHolder a = None | MinHolder a deriving Show
newVal :: (Ord a) => (b, a) -> MinHolder (b, a) -> MinHolder (b, a)
newVal a None = MinHolder a
newVal (a, b) (MinHolder (!x, !y)) =
  if b > y
    then MinHolder (x, y)
    else MinHolder (a, b)
getMin :: MinHolder a -> a
getMin (MinHolder m) = m
