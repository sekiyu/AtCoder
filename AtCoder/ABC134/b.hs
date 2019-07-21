main :: IO ()
main = getLine >>= print . solve . map read . words
solve :: [Int] -> Int
solve (n:d:_) = n `div` c + if n `mod` c == 0 then 0 else 1
  where
    c = 2 * d + 1