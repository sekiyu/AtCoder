main :: IO()
main = do
  n <- getLine
  as <- map read . words <$> getLine :: IO([Int])
  print $ sum [solve x | x <- as]

solve :: Int -> Int
solve a
  | a `mod` 2 == 0 = 1 + (solve $ a `div` 2)
  | otherwise = 0
