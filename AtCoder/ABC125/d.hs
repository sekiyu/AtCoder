import Debug.Trace
main :: IO ()
main = do
  n <- readLn :: IO Int
  map read . words <$> getLine >>= print . solve

solve :: [Integer] -> Integer
solve as = if even negatives then sum abss else sum abss - 2 * mn
  where
    negatives = length . filter (<0) $ as
    abss = map abs as
    mn = minimum abss