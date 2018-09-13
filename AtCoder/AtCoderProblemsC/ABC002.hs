import Control.Monad

main :: IO ()
main = do
  ss <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss

solve :: [Int] -> Float
solve (x1:y1:x2:y2:x3:y3:_) = abs $ fromIntegral (a * d - b * c) / 2.0
  where
    a = x2 - x1
    b = y2 - y1
    c = x3 - x1
    d = y3 - y1
