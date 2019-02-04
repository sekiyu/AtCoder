import Data.List

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  xs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n xs

solve n xs = sum . take (length xs - n) $ sort diffs
  where
    sorted = sort xs
    diffs = zipWith (-) (tail sorted) sorted