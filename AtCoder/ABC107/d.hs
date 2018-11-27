-- ABC107 D
import Data.List

main :: IO ()
main = do
  n <- readLn
  xs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n xs

solve :: Int -> [Int] -> Int
solve n xs = median . map median
  . filter (not.null) . continuousPartialSequence $ xs

median :: (Ord a) => [a] -> a
median xs = (!!(n `div` 2)) $ sort xs
  where n = length xs

continuousPartialSequence :: [a] -> [[a]]
continuousPartialSequence xs = [ (take j . drop i $ xs) | i <- [0..(n-1)], j <- [1..(n-i)]]
  where n = length xs
