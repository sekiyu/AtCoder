main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  putStrLn . unwords . map show $ solve' n m


-- naive solution
solve :: Int -> Int -> [Int]
solve n m
  | n * 2 > m || n * 4 < m = [-1, -1, -1]
  | otherwise = head [[x, y, z] | x <- [0..n], y <- [0..(n - x)], z <- [0..(n - x - y)], 2 * x + 3 * y + 4 * z == m, x + y + z == n]

solve' :: Int -> Int -> [Int]
solve' n m
  | n * 2 > m || n * 4 < m = [-1, -1, -1]
  | otherwise = if rest < 0
                then [abs rest, n + rest, 0]
                else [0, n - rest, rest]
    where
      rest = m - n * 3
