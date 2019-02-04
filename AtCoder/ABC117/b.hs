main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  putStrLn $ if solve ss then "Yes" else "No"

solve ls = sum ls > 2 * m
  where m = maximum ls 