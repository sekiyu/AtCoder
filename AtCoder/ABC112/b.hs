-- ABC112 B Time Limit Exceeded
import Control.Monad

main :: IO ()
main = do
  (n:t:_) <- fmap (map read . words) getLine :: IO [Int]
  am <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  solve t am

solve :: Int -> [[Int]] -> IO ()
solve t am
  | null candidates = putStrLn "TLE"
  | otherwise = print . minimum . map head $ candidates
  where
    candidates = filter (\(_:time:_) -> time <= t) $ am
