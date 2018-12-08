-- ABC115 C
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
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  hs <- fmap join $ replicateM n $ map read . words <$> getLine :: IO [Int]

  print $ solve k hs

solve k hs = minimum $ go sorted (drop (k-1) sorted)
  where
    sorted = sort hs
    go _ [] = []
    go (a:as) (b:bs) = (b - a):go as bs