-- ABC032 D - ナップサック問題
import Control.Monad
import Data.List
import qualified Data.IntMap.Strict as M
-- import qualified Data.Map.Strict as M

main :: IO ()
main = do
  (n:w:_) <- fmap (map read . words) getLine :: IO [Int]
  -- vws <- replicateM (fromIntegral n) $ (\(v:w:_) -> (v, w)) . map (toInteger . read) . words <$> getLine :: IO [(Integer, Integer)]
  vws <- replicateM n $ (\(v:w:_) -> (v, w)) . map read . words <$> getLine :: IO [(Int, Int)]
  -- print $ solve w vws
  print $ solveMapDp w vws


-- DP solution using IntMap
solveMapDp :: Int -> [(Int, Int)] -> Int
solveMapDp maxw vws = maximum . M.elems $ dp
  where
    initial = M.singleton 0 0
    dp = foldr f initial vws
    -- dp = foldl' (flip f) initial vws
    f :: (Int, Int) -> M.IntMap Int -> M.IntMap Int
    f (v, w) table = foldr g table $ M.keys table
      where
        g :: Int -> M.IntMap Int -> M.IntMap Int
        g i table
          | i + w > maxw = table
          | otherwise = M.insertWith max (i+w) ((table M.! i) + v) table

-- Definitive solution
solve :: Int -> [(Int, Int)] -> Int
solve w vws = maximum . map (sum . map fst)
  . filter (\c -> w >= (sum . map snd $ c)) $ subsequences vws
