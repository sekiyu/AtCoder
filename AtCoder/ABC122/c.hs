{-# LANGUAGE BangPatterns #-}
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
-- import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST

main :: IO ()
main = do
  (n:q:_) <- fmap (map read . words) getLine :: IO [Int]
  s <- getLine
  lrs <- replicateM q $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  putStr . unlines . map show $ solve s lrs

solve :: String -> [(Int, Int)] -> [Int]
solve s = map count
  where
    counts = countAC s
    countAC s = tail . reverse $ go [0] s
      where
        go xs [] = xs
        go (x:xs) (a:b:c)
          | a == 'A' && b == 'C' = go ((x+1):x:x:xs) c
          | otherwise = go (x:x:xs) (b:c)
        go (x:xs) _ = x:x:xs
    
    countArr = listArray (0, length s) (0:counts)
    count :: (Int, Int) -> Int
    count (l, r) = countArr ! r - countArr ! (l)

