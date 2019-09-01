main :: IO ()
main = getLine >>= print . solve . map read . words
solve :: [Int] -> Int
solve (a:b:_) = head [ i | i <- [0..], i*(a-1) + 1 >= b]