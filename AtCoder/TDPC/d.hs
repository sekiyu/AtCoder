-- TDPC D - サイコロ

main :: IO ()
main = do
  (n:d:_) <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve n d

solve :: Integer -> Integer -> Double
solve n d = last . map (f d) $ [1..n]
  where
    f 1 1 = 1
    f 2 1 = 1 / 2
    f 3 1 = 1 / 3
    f 4 1 = 1 / 6
    f 5 1 = 1 / 6
    f 6 1 = 1 / 6
    f _ 1 = 0
    f d i = (1/6*) . sum $ map (g d i) [1..6]
      where
        g d i j
          | d `mod` j == 0 = f (d `div` j) (i - 1)
          | j `mod` 2 == 0 && d `mod` 2 == 0 = g (d `div` 2) i (j `div` 2)
          | j `mod` 3 == 0 && d `mod` 3 == 0 = g (d `div` 3) i (j `div` 3)
          | otherwise = f d (i - 1)
