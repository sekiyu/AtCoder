import Control.Monad

main :: IO ()
main = do
  n <- readLn
  putStrLn . foldr (++) [] . map show $ solve (n `mod` 30)

solve :: Int -> [Int]
solve n = go j rotated
  where
    i = n `div` 5
    j = n `mod` 5
    rotate 0 as = as
    rotate n (a:as) = rotate (n - 1) (as ++ [a])
    rotated = rotate i [1..6]
    go n (a:as) = (take n as) ++ [a] ++ (drop n as)
