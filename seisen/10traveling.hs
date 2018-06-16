import Data.List
import Control.Monad

main :: IO()
main = do
  n <- getLine
  ds <- replicateM (read n) $ map read . words <$> getLine :: IO([[Int]])
  putStrLn $ if solve ([0, 0, 0]:ds) then "Yes" else "No"

solve :: [[Int]] -> Bool
solve ([t1, x1, y1]:[t2, x2, y2]:s)
  = (innerSolve (t1, x1, y1) (t2, x2, y2)) && solve ([t2, x2, y2]:s)
solve ([_, _, _]:[]) = True

innerSolve :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
innerSolve (t1, x1, y1) (t2, x2, y2)
  = distance <= interval
  && (distance - interval) `mod` 2 == 0
    where distance = abs (x1 - x2) + abs (y1 - y2)
          interval = abs (t1 - t2)
