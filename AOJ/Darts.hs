import Control.Monad
import Data.Functor
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.List

readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  -- ps <- fmap join . replicateM n $ map read . words <$> getLine :: IO [Int]
  -- ps <- replicateM n $ readLn :: IO [Int]
  ps <- fmap join . replicateM n $ readInts
  solve n m ps

solve n m ps
  | n == 0 && m == 0 = return ()
  | otherwise = do
      -- print . naiveSolve m . join $ ps
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
-- 得点を予めソートしておけばこれを探すのはlog(n)
-- したがって最終的にn^2*log(n)
efficientSolve :: Int -> [Int] -> Int
efficientSolve m ps = maximum ???
  where
    -- 配列にしないとlog(n)で計算できない？
    half = sort . map sum . join . map (flip replicateM ps) $ [0..2]

glelem :: Int -> [Int] ->
-- glelem m as = find (<=m) . reverse $ as
glelem m as
  | m < 0 = negate m
  | otherwise =
