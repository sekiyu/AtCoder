import Control.Monad
import Data.Functor

main :: IO ()
main = do
  n <- readLn
  as <- join <$> (replicateM n $ map read . words <$> getLine) :: IO [Int]
  print $ solve as

solve :: [Int] -> Int
solve (a:as) = maxProfit as minBound a
  where
    maxProfit :: [Int] -> Int -> Int -> Int
    maxProfit [] maxProf _ = maxProf
    maxProfit (a:as) maxProf minFx
      = maxProfit as (max maxProf (a - minFx)) (min a minFx)
