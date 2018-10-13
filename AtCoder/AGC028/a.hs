-- AGC028 A
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
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  s <- getLine :: IO String
  t <- getLine :: IO String
  print $ solve n m s t

solve :: Int -> Int -> String -> String -> Int
solve n m s t = if check then l else (-1)
  where
    l = lcm n m
    check = all id $ Map.intersectionWith (==) smap tmap
    ss = map (+1) . map (l `div` n *) $ [0..(n-1)]
    ts = map (+1) . map (l `div` m *) $ [0..(m-1)]
    smap = Map.fromList $ zip ss s :: Map.Map Int Char
    tmap = Map.fromList $ zip ts t
