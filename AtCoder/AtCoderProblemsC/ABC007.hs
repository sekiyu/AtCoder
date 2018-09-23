-- ABC007 C Breadth First Search
import Control.Monad
import qualified Data.Set as Set
import Data.Array

main :: IO ()
main = do
  (r:c:_) <- fmap (map read . words) getLine :: IO [Int]
  (sx:sy:_) <- fmap (map read . words) getLine :: IO [Int]
  (gx:gy:_) <- fmap (map read . words) getLine :: IO [Int]
  ms <- replicateM r $ getLine :: IO [[Char]]
  print $ solve (sx,sy) (gx,gy) $ toMaze (r, c) ms

type Position = (Int, Int)
type Maze = Array Position Bool
toMaze :: (Int, Int) -> [[Char]] -> Maze
toMaze (n, m) = listArray ((1,1), (n,m)) . join . map (map (=='.'))

solve :: Position -> Position -> Maze -> Int
solve start goal maze = bfs (Set.singleton start) 0
  where
    -- initial = listArray (bounds maze) (repeat maxBound)
    bfs :: Set.Set Position -> Int -> Int
    bfs visits n
      | goal `Set.member` visits = n
      | otherwise = bfs (Set.fromList . join . map step $ Set.toList visits) (n + 1)

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
