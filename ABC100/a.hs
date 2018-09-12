main :: IO()
main = do
  [a, b] <- map read . words <$> getLine :: IO([Int])
  putStrLn $ if solve a b then "Yay!" else ":("

solve :: Int -> Int -> Bool
solve a b =
  a <= 8
  && b <= 8
