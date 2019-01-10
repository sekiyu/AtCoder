import Control.Monad

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  print $ solve am

solve :: [[Int]] -> Int
solve am = go am [0,0,0]
  where
    go :: [[Int]] -> [Int] -> Int
    go [] table = maximum table
    go ((a:b:c:_):am) (aa:bb:cc:_) = go am [a + max bb cc, b + max cc aa,c + max aa bb ]