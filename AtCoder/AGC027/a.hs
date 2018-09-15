import Data.List
main :: IO ()
main = do
  (n:x:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve x as

solve :: Int -> [Int] -> Int
solve x as
  | x == sumas = length as
  | x > sumas = length as - 1
  | otherwise = count 0 x (sort as)
  where
    sumas = sum as
    count n acc [] = if acc == 0 then n else (n - 1)
    count n acc (a:as)
      | acc == 0 = n
      | acc < 0 = n - 1
      | otherwise = count (n+1) (acc - a) as
