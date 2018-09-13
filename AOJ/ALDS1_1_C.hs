import Control.Monad
import Data.Functor

main :: IO ()
main = do
  n <- readLn
  as <- join <$> (replicateM n $ map read . words <$> getLine) :: IO [Int]
  print . length . filter isPrime $ as


isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\i -> not $ n `mod` i == 0) (2:[3,5..(1 + (truncate . sqrt $ fromIntegral n))])
