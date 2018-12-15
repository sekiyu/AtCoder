-- ABC099 B
main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve (a:b:_) = hegiht (b - a - 1) - a

hegiht 1 = 1
hegiht n = n + hegiht (n-1)