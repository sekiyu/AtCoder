-- AGC029 B
import Data.List

main :: IO ()
main = do
  n <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve as

solve :: [Int] -> Int
solve as = go $ sort as
  where
    go :: [Int] -> Int
    go [] = 0
    go sorted
      | elem beki $ init sorted = 1 + (go . delete beki . init $ sorted)
      | otherwise = go $ init sorted
      where 
        m = last sorted
        beki = (next2pow m) - m

next2pow x = head . filter (x<) $ map (2^) [1..]