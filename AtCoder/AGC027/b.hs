import Control.Monad

main :: IO ()
main = do
  (n:x:_) <- fmap (map read . words) getLine :: IO [Int]
  xs <- fmap (map read . words) getLine :: IO [Int]
  print $ (solve x (reverse xs)) + n * x

solve :: Int -> [Int] -> Int
solve x (xi:[]) = xi + 2^2 * xi + x
solve x (xi:xnext:xs) =
