-- ABC112 A Programming Education
main :: IO ()
main = do
  n <- readLn
  solve n

solve :: Int -> IO ()
solve 1 = putStrLn "Hello World"
solve 2 = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  print $ a + b
