-- ABC107 Candles
import Data.Array

main :: IO ()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  xs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n k xs

solve :: Int -> Int -> [Int] -> Int
solve n k xs = foldr min (maxBound :: Int) $ map f [1..(n - k + 1)]
  where
    xarray = listArray (1, n) xs
    f i | left * right < 0 = min (-2 * left + right) (-left + 2 * right)
        | otherwise = max (-left) right
        where
          left = xarray!i
          right = xarray!(i+k-1)
