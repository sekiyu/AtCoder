import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn . showbm2 . solve $ n
  -- print . limn $ n

solve :: Int -> [Int]
solve n = head . join $ [ [ cand | cand <- greedCandidates i, bm2 cand == n]| i <- [0..]]

greedCandidates n = replicateM n [0, 1]

bm2 as = sum $ zipWith (\a i -> a * (-2) ^ i) (reverse as) [0..]

showbm2 :: [Int] -> String
showbm2 as = concat $ map show as 



limn n = head [i | i <- [0..], 2 ^ i > abs n]
