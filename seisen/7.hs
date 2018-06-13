import Data.List

main :: IO()
main = do
  n <- getLine
  as <- map read . words <$> getLine
  let sorted = sort as
  let (a, b) = dualSum sorted (0, 0)
  print . abs $ b - a

dualSum :: Num a => [a] -> (a, a) -> (a, a)
dualSum (x:y:xs) (a, b) = dualSum xs (a + y, b + x)
dualSum (x:[]) (a, b) = (a, b + x)
dualSum [] s = s
