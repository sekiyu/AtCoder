import Control.Monad

main :: IO ()
main = do
  n <- readLn
  dm <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  q <- readLn
  ps <- replicateM q $ readLn :: IO [Int]
  mapM_ (print . solve dm) ps

solve :: [[Int]] -> Int -> [Int]
solve dm p = dm !! 1
-- (floor . sqrt . fromIntegral p)
