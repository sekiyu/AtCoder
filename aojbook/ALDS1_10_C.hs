import Control.Monad

main :: IO ()
main = do
  n <- readLn
  xs <- replicateM (2 * n) getLine :: IO [String]
  solve xs

solve :: [String] -> IO ()
solve [] = return ()
solve (x:y:z) = do
  print $ lcs x y 0
  solve z

lcs :: String -> String -> Int -> Int
z1zlcs xs [] v = v
lcs [] ys v = v
lcs (x:xs) (y:ys) v
  | x == y = lcs xs ys (v + 1)
  | otherwise = max (lcs xs (y:ys) v) (lcs (x:xs) ys v)
