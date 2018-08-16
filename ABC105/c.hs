import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  -- putStrLn . showbm2 . solve $ n
  putStrLn . showbm2 . reverse . solve' $ n

-- brute force search
-- examine all the candidates
solve :: Int -> [Int]
solve 0 = [0]
solve n = head . join $ [ [ cand | cand <- greedCandidates i, bm2 cand == n]| i <- [0..]]

greedCandidates n = replicateM n [0, 1]

bm2 as = sum $ zipWith (\a i -> a * (-2) ^ i) (reverse as) [0..]

showbm2 :: [Int] -> String
showbm2 as = concat $ map show as

-- solve according to the commentary
solve' :: Int -> [Int]
solve' 0 = [0]
solve' 1 = [1]
solve' n = a : solve' ((n - a) `div` (-2))
  where
    a = n `mod` 2
