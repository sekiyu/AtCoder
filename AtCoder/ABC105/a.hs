main :: IO ()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ if n `mod` k == 0 then 0 else 1
