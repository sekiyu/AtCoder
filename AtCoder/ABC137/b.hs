main = getLine >>= putStrLn . unwords . map show . solve . map read . words
solve (k:x:_) = [(x - k + 1)..(x + k - 1)]