import Control.Monad

main :: IO ()
main = do
  n <- readLn
  xs <- replicateM (2 * n) getLine :: IO [String]
  solve xs

solve :: [String] -> IO ()
solve [] = return ()
solve (x:y:z) = do
--  print $ lcs x y 0
  print $ lcs' x y []
  solve z

lcs :: String -> String -> Int -> Int
lcs xs [] v = v
lcs [] ys v = v
lcs (x:xs) (y:ys) v
  | x == y = lcs xs ys (v + 1)
  | otherwise = max (lcs xs (y:ys) v) (lcs (x:xs) ys v)


lcs' :: String -> String -> [Int] -> Int
lcs' x y table
  | i >= m * n = head table
  | x !! j == y !! k = lcs' x y $ (lu + 1):table
  | otherwise = lcs' x y $ (max l u):table
  where
    m = length x
    n = length y
    i = length table
    j = mod i m
    k = div i m
    l = if j > 0 then head table else 0
    u = if k > 0 then table !! (m - 1) else 0
    lu = if j > 0 && k > 0 then table !! m else 0
