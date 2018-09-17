-- ARC031 B Reclaim
import Control.Monad
import Data.Array
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  -- ss <- lines <$> getContents :: IO [String]
  ss <- replicateM 10 getLine :: IO [String]
  -- let new = (reclaimPatterns . to2DArray . toIsLands $ ss)!!38
  -- print new
  -- print $ isOneLand new
  putStrLn $ if solve ss then "YES" else "NO"

solve :: [String] -> Bool
solve ss = any isOneLand $ reclaimPatterns . to2DArray . toIsLands $ ss

toIsLands :: [[Char]] -> [[Bool]]
toIsLands = map (map (=='o'))

to2DArray :: [[Bool]] -> Array (Int, Int) Bool
to2DArray isLands = listArray ((1,1), (n,m)) $ join isLands
  where
    n = length isLands
    m = length $ head isLands

type World = Array (Int, Int) Bool

isOneLand :: World -> Bool
isOneLand w = dfs w [first] Set.empty
  where
     n = length . filter id . elems $ w
     (first, _) = head . filter (\(_, b) -> b) $ assocs w

     dfs :: World -> [(Int, Int)] -> Set.Set (Int, Int) -> Bool
     -- dfs _ [] visited = traceShow visited (n == (Set.size visited))
     dfs _ [] visited = n == (Set.size visited)
     dfs w (n:nexts) visited = dfs w (adjLand ++ nexts) (Set.insert n visited)
      where
        (x, y) = n
        candidates = filter (\(x,y) -> 0<x && x<=10 && 0<y && y <=10) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
        adjLand = [ c | c <- candidates, w!c , Set.notMember c visited]

reclaimPatterns :: World -> [World]
reclaimPatterns w = catMaybes . map (reclaim w) $ indices w
  where
    reclaim :: World -> (Int, Int) -> Maybe World
    reclaim w i
      | w!i = Nothing
      | otherwise = Just $ w // [(i, True)]
