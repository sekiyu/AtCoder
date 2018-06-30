main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ fibonacci n

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fib (n - 2) [1, 1]
  where
    fib 0 (x:y:_) = x + y
    fib i (x:y:z) = fib (i - 1) $ (x + y):x:y:z
