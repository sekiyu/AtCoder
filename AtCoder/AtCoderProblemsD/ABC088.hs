-- ABC088 D Grid Repainting
import Control.Monad
import qualified Data.Set as Set
import Data.Maybe
import Data.Array
import Data.List
import Debug.Trace

main :: IO ()
main = do
  (h:w:_) <- fmap (map read . words) getLine :: IO [Int]
  ms <- replicateM h $ getLine :: IO [[Char]]
  -- print $ toMaze (h, w) ms
  print . solve (1, 1) (h, w) . toMaze (h, w) $ ms

type Position = (Int, Int)
type Maze = Array Position Bool
toMaze :: (Int, Int) -> [[Char]] -> Maze
toMaze (n, m) = listArray ((1,1), (n,m)) . join . map (map (=='.'))

solve :: Position -> Position -> Maze -> Int
solve start goal maze = n - 1 - (bfs (Set.singleton start) 0)
  where
    n = length . filter id $ elems maze
    bfs :: Set.Set Position -> Int -> Int
    bfs visits n
      | goal `Set.member` visits = n
      | otherwise = bfs (Set.fromList . join . map step $ Set.toList visits) (n + 1)
      -- | otherwise = traceShow visits $ bfs (Set.fromList . join . map step $ Set.toList visits) (n + 1)

    step :: Position -> [Position]
    step (x,y) = validate [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

    validate :: [Position] -> [Position]
    validate = filter (\p -> isInRange p (bounds maze) && maze!p)
    isInRange :: Position -> (Position, Position) -> Bool
    isInRange p (l, u) = lx <= px
                        && px <= ux
                        && ly <= py
                        && py <= uy
      where
        (px,py) = p
        (lx,ly) = l
        (ux,uy) = u
