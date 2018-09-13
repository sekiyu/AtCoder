import Control.Monad

main :: IO()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  _ <- getLine
  ms <- fmap (map read . words) getLine :: IO [Int]
  solve as ms

solve :: [Int] -> [Int] -> IO()
solve _ [] = return ()
solve as (m:ms) = do
  putStrLn $ if innerSolve as m then "yes" else "no"
  solve as ms
    where
      innerSolve :: [Int] -> Int -> Bool
      innerSolve _ 0 = True
      innerSolve [] m = False
      innerSolve (a:as) m = innerSolve as m || innerSolve as (m - a)

      -- These codes are very slow.
      --innerSolve as m = 0 < length [1 | is <- replicateM (length as) [0,1], (sum $ zipWith (*) is as) == m]
      --innerSolve as m = any (\switch -> (sum $ zipWith (*) switch as) == m) $ replicateM (length as) [0,1]
