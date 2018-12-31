-- AGC030 A
main :: IO ()
main = do
  (a:b:c:_) <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve a b c

-- solve :: Int -> Int -> Int -> Int
solve a b c = b + (min c (a + b + 1))