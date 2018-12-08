-- ABC115 B
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
  xs <- fmap join $ replicateM n $ map read . words <$> getLine :: IO [Int]
  print $ solve xs

solve xs = sum xs - m `div` 2
  where
    m = maximum xs
