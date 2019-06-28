main :: IO()
main = do
  [n, y] <- map read . words <$> getLine
  putStrLn . unwords . map show $ solve n y

solve :: Int -> Int -> [Int]
solve n y = [a, b, c]
  where bs = [b | b <- [0..n], (y - 1000 * n - 4000 * b) `mod` 9000 == 0]
        ans = [(a, b, n - b - a) | b <- bs, a <- [(y - 1000 * n - 4000 * b) `div` 9000], a >= 0, c >= 0]
        (a, b, c) = if length ans == 0 then (-1, -1, -1) else head ans
