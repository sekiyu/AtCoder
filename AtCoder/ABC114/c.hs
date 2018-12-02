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
  print $ solve n

solve :: Int -> Int
solve n = length . filter (<=n) . join $ map candidates [3..(keta n + 1)]

candidates :: Int -> [Int]
candidates n = map (sum . zipWith (*) tens) . validate $ replicateM n [3,5,7]

tens = (map (10^) [0..])

validate = filter isValid
  where isValid ls = (3 `elem` ls) && (5 `elem` ls) && (7 `elem` ls)

keta :: Int -> Int
keta n = round . (logBase 10) . fromIntegral . head $ filter (> n) tens
