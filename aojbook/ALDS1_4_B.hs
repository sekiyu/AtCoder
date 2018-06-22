main :: IO()
main = do
  _ <- fmap read getLine :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  _ <- fmap read getLine :: IO Int
  ts <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss ts

solve :: [Int] -> [Int] -> Int
solve ss = foldl (\acc t -> acc + binarySearch t ss 0 (length ss - 1)) 0

binarySearch :: Int -> [Int] -> Int -> Int -> Int
binarySearch t ss b e
  | t == mid = 1
  | b == e || b > e  = 0
  | t < mid = binarySearch t ss b (i - 1)
  | t > mid = binarySearch t ss (i + 1) e
  where
    i = div (b + e) 2
    mid = ss !! i
