main :: IO()
main = do
  n <- getLine
  as <- map read . words <$> getLine
  print . length . takeWhile (== True) $ map (solve as) [1..]

solve :: [Int] -> Int -> Bool
solve as b = (sum $ map (`mod` 2^b) as) == 0
