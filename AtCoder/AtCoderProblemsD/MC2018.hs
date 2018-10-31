Maximum cup 2018 D - Many Go Round
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet


main :: IO ()
main = do
  (n:m:l:x:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  putStrLn $ if solve n m l x as then "Yes" else "No"

-- DPでテーブルに何周目かという情報を持たせる予定
solveDp :: Int -> Int -> Int -> Int -> [Int] -> Bool
solveDp n m l x as = IntMap.member l dp && dp IntMap.! l <= x
  where
    dp = foldr f (IntMap.singleton 0 0) as
    f :: Int -> IntMap.IntMap -> IntMap.IntMap
    f _ dp = dp

-- definitive solution
solve :: Int -> Int -> Int -> Int -> [Int] -> Bool
solve n m l x as = any (\s -> s `mod` m == l)
  . filter (<= m*x) . map sum $ subsequences as
