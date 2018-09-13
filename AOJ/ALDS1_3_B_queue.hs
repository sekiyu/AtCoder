import Control.Monad
import Data.Graph.Inductive.Internal.Queue

main :: IO()
main = do
  [n, q] <- fmap (map read . words) getLine :: IO[Int]
  ps <- replicateM n $ fmap words getLine :: IO [[String]]
  mapM_ (putStrLn . unwords)  $ solve ps q

solve :: [[String]] -> Int -> [[String]]
solve ps q = innerSolve q 0 $ intoQueue ps mkQueue
  where
    innerSolve :: Int -> Int -> Queue (String, Int) -> [[String]]
    innerSolve q elapsed que
      | queueEmpty que = []
      | v <= q = [w, show finished]:(innerSolve q finished queG)
      | v > q  = innerSolve q (elapsed + q) (queuePut (w, v - q) queG)
      where
        ((w, v), queG) = queueGet que
        finished = elapsed + v

intoQueue :: [[String]] -> Queue (String, Int) -> Queue (String, Int)
intoQueue [] que = que
intoQueue (p:ps) que = intoQueue ps $ queuePut (w, read v) que
  where
    (w:v:_) = p
