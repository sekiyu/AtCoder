import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  map read . words <$> getLine >>= print . solve

solve as = maximum $ zipWith gcd ls rs
  where
    ls = init $ scanl' gcd 0 as
    rs = tail $ scanr gcd 0 as
    -- rs = tail $ reverse . scanl' gcd 0 $ reverse as
