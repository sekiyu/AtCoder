main :: IO ()
main = do
  as <- fmap (map read . words) getLine :: IO [Int]
  print . solve $ as

solve :: [Int] -> Int
solve (a:b:c:_) = abs (a - b) + abs (b - c) + abs(c - a) - abs (a - b) `max` abs (b - c) `max` abs(c - a)
