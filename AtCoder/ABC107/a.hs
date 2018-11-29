-- ABC107 A
main :: IO ()
main = do
  (n:i:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n i

solve :: Int -> Int -> Int
solve n i = n - i + 1
