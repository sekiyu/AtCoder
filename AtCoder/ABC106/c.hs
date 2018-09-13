main :: IO ()
main = do
  ks <- getLine
  n <- readLn
  putStrLn $ solve n ks

solve 1 (k:ks) = [k]
solve n (k:ks)
  | k == '1' = solve (n - 1) ks
  | otherwise = [k]
