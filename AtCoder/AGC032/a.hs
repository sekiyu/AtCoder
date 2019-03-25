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
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  let ans = solve n ss
  if isNothing ans
    then putStrLn "-1"
    else putStr . unlines . map show $ fromJust ans
  {-
  let ans = naive n ss 
  putStr . unlines . map show $ if null ans then [-1] else head ans
  -}

solve :: Int -> [Int] -> Maybe [Int]
solve n ss = dfs (vanishable ss) [] ss
  where
    -- nexts = map fst . filter (\(i, j) -> i == j) $ zip ss [1..]
    dfs :: [Int] -> [Int] -> [Int] -> Maybe [Int]
    dfs _ visiteds [] = Just visiteds
    dfs [] visiteds _ = Nothing
    -- dfs (n:ns) vs ss = traceShow (n:ns, vs, ss) $ dfs ns (n:vs) $ unins n ss
    dfs (n:ns) vs ss
      | null removed = dfs ns (n:vs) []
      | null nexts = dfs ns vs ss
      | otherwise  = dfs (ns ++ nexts) (n:vs) removed
      where
        removed = unins n ss
        nexts = vanishable removed

vanishable :: [Int] -> [Int] 
vanishable ss = map fst . filter (\(i, j) -> i == j) $ zip ss [1..]

ins :: Int -> [Int] -> [Int]
ins a as = let (l, r) = splitAt (a - 1) as
           in l ++ [a] ++ r
unins :: Int -> [Int] -> [Int]
unins a as = let (l, r) = splitAt a as
             in (init l) ++ r

operate = go []
  where
    go :: [Int] -> [Int] -> [Int]
    go bs [] = reverse bs
    go [] (a:as) = go [a] as
    go bs (a:as) = let (l, r) = splitAt (length bs - a + 1) bs
                   in go (l ++ [a] ++ r) as


naive n ss = [c | c <- candidates n, operate c == ss]
  where
    candidates n = foldr1 (\x acc -> (++) <$> x <*> acc) $ map inc [1..n]
      where
        inc i = [ [j] | j <- [1..i]]

