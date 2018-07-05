import Data.List

main :: IO()
main = do
  _ <- fmap read getLine :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  _ <- fmap read getLine :: IO Int
  ts <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss ts 0

solve :: [Int] -> [Int] -> Int -> Int
--solve ss = foldl' (\acc t -> acc + binarySearch t ss 0 (length ss - 1)) 0
--solve ss ts = length [1 | t <- ts, binarySearch t ss 0 (length ss - 1)]
solve _ [] n = n
solve ss (t:ts) n = solve ss ts $ n + if elem t ss then 1 else 0

binarySearch :: Int -> [Int] -> Int -> Int -> Bool
binarySearch t ss b e
  | t == mid = True
  | b == e || b > e  = False
  | t < mid = binarySearch t ss b (i - 1)
  | t > mid = binarySearch t ss (i + 1) e
  where
    i = div (b + e) 2
    mid = ss !! i
