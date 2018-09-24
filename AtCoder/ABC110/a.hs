-- ABC110 A Maximize the Formula
main :: IO ()
main = do
  ss <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss

solve :: [Int] -> Int
solve (a:b:c:_) = maximum [(a + 10 * b + c), (10 * a + b + c), (a + b + 10* c)]
