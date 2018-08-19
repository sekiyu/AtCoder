import Control.Monad

main :: IO ()
main = do
  (a:b:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ (a - 1) * (b - 1)
