main :: IO ()
main = do
  (t:x:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ solve t x

solve t x = (fromIntegral t) / (fromIntegral x)