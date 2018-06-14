import Data.Char

main :: IO()
main = do
  [n, a, b] <- map read . words <$> getLine
  print . sum $ [x | x <- [1..n], sumval x >= a,  sumval x <= b]
    where sumval y = sum $ map digitToInt $ show y
