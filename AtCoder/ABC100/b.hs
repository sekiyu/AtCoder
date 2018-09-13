import Data.List

main :: IO()
main = do
  [d, n] <- map read . words <$> getLine :: IO([Int])
  print $ solve d n

solve d 100 = solve d 101
solve d n = [100^d * a | a <- [1..101]] !! (n - 1)
