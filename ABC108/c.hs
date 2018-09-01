import Control.Monad

main :: IO ()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ solve' n k

solve :: Int -> Int -> Int
solve n k = length $ filter (validate k) (replicateM 3 [1..n])
  where validate k a = (a !! 0 + a !! 1) `mod` k == 0 && (a !! 1 + a !! 2) `mod` k == 0 && (a !! 2 + a !! 0) `mod` k == 0

solve' n k
  | k `mod` 2 == 1 = m^3 -- odd
  | otherwise = m^3 + p^3
    where
      m = n `div` k
      p = if n `mod` k < k `div` 2
          then m
          else m + 1
