main :: IO ()
main = do
  ss <- fmap (map read . words) getLine :: IO [Int]
  putStrLn . unwords . map show $ solve ss

solve :: [Int] -> [Int]
solve (x1:y1:x2:y2:_) = x3:y3:x4:y4:[]
  where
    x3 = x2 - y2 + y1
    y3 = y2 + x2 - x1
    x4 = x1 - y2 + y1
    y4 = y1 + x2 - x1
