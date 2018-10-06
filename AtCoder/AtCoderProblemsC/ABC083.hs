-- ABC083 C - Multiple Gift

main :: IO ()
main = do
  (x:y:_) <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve x y

solve x y
  | x > y = 0
  | otherwise = 1 + solve (2*x) y
