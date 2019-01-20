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
  ss <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss

solve :: [Int] -> Int
solve (a:b:c:_) = a * b `div` 2