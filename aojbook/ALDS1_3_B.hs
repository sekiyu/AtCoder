import Control.Monad

main :: IO()
main = do
  [n, q] <- fmap (map read . words) getLine :: IO[Int]
  ps <- replicateM n $ fmap words getLine :: IO [[String]]
  mapM_ (putStrLn . unwords)  $ solve ps q 0

solve :: [[String]] -> Int -> Int -> [[String]]
solve [] _ _ = []
solve (p:ps) q elapsed
  | v <= q = [w, show finished]:(solve ps q finished)
  | v > q  = solve (ps ++ [[w, show $ v - q]]) q $ elapsed + q
  where
    [w, vstr] = p
    v = read vstr
    finished = elapsed + v
