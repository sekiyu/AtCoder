main :: IO()
main = do
  _ <- fmap read getLine :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  _ <- fmap read getLine :: IO Int
  ts <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss ts 0

solve :: [Int] -> [Int] -> Int -> Int
solve _ [] i = i
solve ss (t:ts) i = solve ss ts $ if t `elem` ss then i + 1 else i
