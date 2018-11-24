-- DISCO B - チップ・ストーリー　～漆黒編～
main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n
  | n `mod` 2 == 0 = 4 * (sum [1..(n `div` 2 - 1)])
  | otherwise = 2 * (sum [1,3..(n-2)]) - n + 2
