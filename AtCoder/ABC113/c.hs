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
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  pys <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  putStr $ unlines $ solve n pys

-- solve :: It -> [[Int]] -> [String]
solve n pys = map (\(p:y:_) -> (seikei p) ++ seikei (dicdic IntMap.! p IntMap.! y)) pys
  where
    dic = foldr (\(p:y:_) acc -> IntMap.insertWith (++) p [y] acc) IntMap.empty pys :: IntMap.IntMap [Int]
    dicdic = IntMap.map (\l -> IntMap.fromList (zip (sort l) [1..])) dic

seikei :: Int -> String
seikei x = (replicate (6 - l) '0') ++ xstr
  where
    xstr = show x
    l = length xstr


    {-
    num p y = count y $ sortedDic IntMap.! p
      where
        count i [] = 0
        count i (l:ls)
          | i == l = 1
          | otherwise = 1 + count i ls
    -}
