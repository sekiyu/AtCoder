main :: IO ()
main = do
  (n:x:_) <- fmap (map read . words) getLine :: IO [Int]
  xs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve x xs

solve :: Int -> [Int] -> Int
solve x xs = foldr1 gcd abss
  where
    abss = map (\y -> abs (y - x)) xs
