import Control.Monad

main :: IO()
main = do
  [n, q] <- map read . words <$> getLine
  ps <- replicateM n $ words <$> getLine :: IO [[String]]
  mapM_ (\ls -> do
            putStr (head ls)
            putStr " ";
            putStrLn (last ls)) $ solve ps q 0

solve :: [[String]] -> Int -> Int -> [[String]]
solve [] _ _ = []
solve (p:ps) q elapsed
  | v <= q = [w, show finished]:(solve ps q finished)
  | v > q  = solve (ps ++ [[w, show $ v - q]]) q $ elapsed + q
  where
    [w, vstr] = p
    v = read vstr
    finished = elapsed + v
