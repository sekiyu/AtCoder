-- ABC032 D - ナップサック問題
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
  (n:w:_) <- fmap (map read . words) getLine :: IO [Int]
  vws <- replicateM n $ (\(v:w:_) -> (v, w)) . map read . words <$> getLine :: IO [(Int, Int)]
  print $ solve w vws


-- Definitive solution
solve :: Int -> [(Int, Int)] -> Int
solve w vws = maximum . map (sum . map fst)
  . filter (\c -> w >= (sum . map snd $ c)) $ subsequences vws
