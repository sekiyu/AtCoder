main :: IO ()
main = do
  readLn >>= solve
  
solve :: Int -> IO ()
solve n = print $ a * (n - a)
  where a = n `div` 2
