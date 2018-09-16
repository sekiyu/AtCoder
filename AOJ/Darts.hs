import Control.Monad
import Data.Functor
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.List
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Debug.Trace

readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  -- ps <- replicateM n $ readLn :: IO [Int]
  ps <- fmap join . replicateM n $ readInts
  solve n m ps

solve n m ps
  | n == 0 && m == 0 = return ()
  | otherwise = do
      -- print . naiveSolve m $ ps
      print . efficientSolve m $ ps
      main

naiveSolve :: Int -> [Int] -> Int
naiveSolve m ps = maximum . filter (<=m) . map sum $ candidates
  where
    candidates = join . map (flip replicateM ps) $ [0..4]

-- 4本での得点でなく2本の得点*2を考え、それぞれp1,p2とする
-- p1とp2の組み合わせの全探索では結局n^4
-- p1を固定したとき、m - p1より小さい最大のp2を探せばよい
-- 得点を予めソートしておけばこれを探すのはO(log n)
-- したがって最終的にO(n^2*log n)
efficientSolve :: Int -> [Int] -> Int
efficientSolve m ps = maximum $ findPair half
  where
    -- 配列にしないとO(log n)で計算できない？
    -- SetでOK? Set.lookupLEはO(log n)
    half = map sum . join . map (flip replicateM ps) $ [0..2]
    halfSet = Set.fromList half
    findPair [] = []
    findPair (a:as)
      | le == Nothing = (if a <= m then [a] else [])
                        ++ findPair as
      | otherwise = ((fromJust le) + a):findPair as
      where le = Set.lookupLE (m - a) halfSet
