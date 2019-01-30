import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
import Data.Array

main :: IO ()
main = do
  n <- readLn
  abs <- replicateM n $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Integer, Integer)]
  print $ solve n abs


solve n abs = (sumodd cs) - sumb
  where
    sumb = foldr (\(a,b) acc -> acc + b) 0 abs
    cs = reverse . sort $ map (\(a,b) -> a + b) abs
    sumodd [] = 0
    sumodd (a:[]) = a
    sumodd (a:aa:aaa) = a + (sumodd aaa)


-- 誤答
-- ただしmonoidでOrderingを結合したのは勉強になった
-- solve' :: [(Integer, Integer)] -> Integer
solve' n abs = traceShow tsan $ (sum . map (\(a,_,_) -> a) $ tsan) - (sum . map (\(_,b,_) -> b) $ asan)
  where
    xs = zipWith (\(a,b) c -> (a,b,c)) abs $ map (\(a,b) -> a - b) abs
    abcs = sortBy (\(a,b,c) (x,y,z) -> (z `compare` c) `mappend` (x `compare` a) `mappend` (b `compare` y)) xs
    (tsan, asan) = splitAt (n `div` 2 + n `mod` 2) abcs

