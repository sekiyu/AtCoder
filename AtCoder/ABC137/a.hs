main = getLine >>= print . solve . map read . words
solve (a:b:_) = maximum [a + b, a - b, a * b]