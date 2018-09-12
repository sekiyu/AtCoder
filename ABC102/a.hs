main = do
  n <- readLn :: IO Int
  print $ if mod n 2 == 0 then n else 2 * n
