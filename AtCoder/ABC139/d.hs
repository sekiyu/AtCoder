main :: IO ()
main = do
  n <- readLn :: IO Integer
  print $ solve n

solve n = n * (n-1) `div` 2