import Control.Monad

main :: IO ()
main = do
  (r:g:b:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ solve r g b

solve :: Int -> Int -> Int -> Int
solve r g b = count r + count g + count b


count n = move k + move l
  where
    k = (n - 1) `div` 2
    l = n - 1 - k


move 0 = 0
move n = n + move (n - 1)
