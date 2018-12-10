-- ABC109 A
main :: IO ()
main = getLine >>= putStrLn . solve . map read . words

solve :: [Int] -> String
solve (a:b:_) = if (a * b) `mod` 2 == 0
                then "No"
                else "Yes"
