-- ABC088 D Grid Repainting
import Control.Monad
import qualified Data.Set as Set
import Data.Maybe
import Data.Array
import Debug.Trace
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  (h:w:_) <- fmap (map read . words) getLine :: IO [Int]
  ms <- replicateM h $ B.getLine :: IO [B.ByteString]
  -- print $ toMaze (h, w) ms
  print . solve (1, 1) (h, w) . toMaze (h, w) $ ms

type Position = (Int, Int)
type Maze = Array Position Bool
toMaze :: (Int, Int) -> [B.ByteString] -> Maze
toMaze (n, m) = listArray ((1,1), (n,m)) . join . map (map (=='.') . B.unpack)

solve :: Position -> Position -> Maze -> Int
solve start goal maze = numOfWhite - 1 - (bfs (Set.singleton start) (Set.singleton start) 0)
  where
    numOfWhite = length . filter id $ elems maze
    bfs :: Set.Set Position -> Set.Set Position -> Int -> Int
    bfs visiting visited n
      | visiting == Set.empty = numOfWhite
      | goal `Set.member` visiting = n
      | otherwise = bfs visit (Set.union visited visit) (n + 1)
        where
          adjacents = Set.fromList . join . map step $ Set.toList visiting
          visit = adjacents Set.\\ visited

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
