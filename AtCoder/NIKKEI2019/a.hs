main :: IO ()
main = do
  (n:a:b:_) <- fmap (map read . words) getLine :: IO [Int]
  putStrLn . unwords . map show $ solve n a b

solve :: Int -> Int -> Int -> [Int]
solve n a b = [x, y]
  where
    x = min a b
    y = max 0 (a + b - n)