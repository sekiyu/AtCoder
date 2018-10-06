-- ABC009 C - 辞書式順序ふたたび
import Data.List

main :: IO ()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  s <- getLine
  putStrLn $ solve k s

solve :: Int -> String -> String
solve k s = go k s sorted
  where
    sorted = sort s
    go :: Int -> String -> String -> String
    go _ [] _ = []
    go _ _ [] = []
    go k (s:ss) (h:hh)
      | k > n = h:hh -- kの方が大きければsortしたものを返す
      | s == h = s:go k ss (sort hh)
      | k + count >= n = h:go (k - 1) ss (sort hh)
      | otherwise = go k (s:ss) (hh ++ [h])
      where
        n = 1 + length ss
        count = numCommonElements ss hh

numCommonElements :: (Ord a) => [a] -> [a] -> Int
numCommonElements as bs = inner (sort as) (sort bs)
  where
    inner :: (Ord a) => [a] -> [a] -> Int
    inner [] _ = 0
    inner _ [] = 0
    inner (a:as) (b:bs)
      | a == b = 1 + inner as bs
      | a > b = inner (a:as) bs
      | a < b = inner as (b:bs)


naive :: Int -> String -> String
naive k s = head . filter validate . sort . permutations $ s
  where
    validate :: String -> Bool
    validate ss = (length . filter not $ zipWith (==) ss s) <= k
