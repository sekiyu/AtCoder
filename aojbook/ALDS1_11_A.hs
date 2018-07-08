import Control.Monad

main :: IO ()
main = do
  n <- readLn
  gs <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  showAdjMatrix $ solve gs

solve :: [[Int]] -> [[Int]]
solve gs = innerSolve gs $ length gs
  where
    innerSolve :: [[Int]] -> Int -> [[Int]]
    innerSolve [] _ = []
    innerSolve (x:xs) n = (makeRow x n) : (innerSolve xs n)

    makeRow :: [Int] -> Int -> [Int]
    makeRow (u:k:vs) n = [if x `elem` vs then 1 else 0 | x <- [1..n]]

showAdjMatrix :: [[Int]] -> IO ()
showAdjMatrix [] = return ()
showAdjMatrix (m:ms) = do
  putStrLn $ unwords . map show $ m
  showAdjMatrix ms
