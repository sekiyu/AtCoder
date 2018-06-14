
main :: IO()
main = do
  [a, b] <- map read . words <$> getLine
  putStrLn $ if (a * b) `mod` 2 == 1 then "Odd" else "Even"
