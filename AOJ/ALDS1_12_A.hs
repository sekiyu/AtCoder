import Control.Monad
import Data.Functor
import Data.List

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO([[Int]])
  let filled = map (map (\w -> if w == -1 then maxBound else w)) am
  print . solve $ filled

solve :: [[Int]] -> Int
solve am = sum . map snd $ mst am [(0, 0)]

type Node = Int
type Weight = Int
type AdjMatrix = [[Weight]]
type SpanningTree = [(Node, Weight)]

mst :: AdjMatrix -> SpanningTree -> SpanningTree
mst am st
  | length am <= length st = st
  | otherwise = mst am (nearest:st)
  where
    spanned = map fst st
    neighbors = [(i, (am !! j) !! i)
      | i <- [0..(length am - 1)], j <- spanned, i `notElem` spanned]
    nearest = minimumBy (\x y -> compare (snd x) (snd y)) neighbors
