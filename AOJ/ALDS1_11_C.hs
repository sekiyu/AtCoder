import           Control.Monad
import qualified Data.Map.Strict as Map

type Graph = [[Int]]
type AdjMatrix = [[Bool]]

main :: IO ()
main = do
  n <- readLn
  gs <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  solve gs

solve :: Graph -> IO ()
solve gs = do
  let visits = bfs (toAdjMatrix gs) [0] (Map.singleton 0 0)
  forM_ [0..(length gs - 1)]
        (\i -> let time = Map.findWithDefault (-1) i visits
               in putStrLn $ (show (i + 1)) ++ " " ++ (show time))

bfs :: AdjMatrix -> [Int] -> Map.Map Int Int -> Map.Map Int Int
bfs _ [] vs = vs
bfs gs qs vs = bfs gs (adjs ++ init qs) newvs
  where
    q = last qs
    edges = gs !! q
    adjs = [i | i <- [0..(length gs - 1)], edges !! i, notElem i $ Map.keys vs]
    Just time = Map.lookup q vs
    newvs = foldr (\adj vs -> Map.insert adj (time + 1) vs) vs adjs


toAdjMatrix :: Graph -> AdjMatrix
toAdjMatrix gs = scanColumn gs $ length gs
  where
    scanColumn :: Graph -> Int -> AdjMatrix
    scanColumn [] _ = []
    scanColumn (x:xs) n = (makeRow x n) : (scanColumn xs n)

    makeRow :: [Int] -> Int -> [Bool]
    makeRow (u:k:vs) n = [if x `elem` vs then True else False | x <- [1..n]]
