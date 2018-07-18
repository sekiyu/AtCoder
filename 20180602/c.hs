
main :: IO()
main = do
  [n, d] <- map read . words <$> getLine :: IO [Int]
  xs <- map read . words <$> getLine :: IO [Int]
  print . sum $ map (countUp d xs) [0..(n - 1)]

countUp :: Int -> [Int] -> Int -> Int
countUp d xs i = b - a
    where
      left = i - lteqNum (xs !! i - d) xs + if i == (length xs - 1) then 1 else 0
      right = lteqNum (xs !! i + d) xs - i - 1
      a = right * (right - 1) `div` 2
      b = left * right

lteqNum :: Int -> [Int] -> Int
lteqNum _ [] = 0
lteqNum x xs
  | last xs <= x = n
  | head xs > x  = 0
  | xmid <= x && (xs !! (mid + 1)) > x = mid + 1
  | x < xmid  = lteqNum x $ take mid xs
  | x > xmid  = (mid+1+) . lteqNum x . reverse
    . take (if (mod n 2 == 0) then mid + 1 else mid) $ reverse xs
    where
      n = length xs
      mid = n `div` 2
      xmid = xs !! mid
