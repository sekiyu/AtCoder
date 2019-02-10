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
  (k:a:b:_) <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve k a b

solve k a b
  | k <= a = k + 1
  | a + 2 > b = k + 1
  | (k - a) `mod` 2 == 1 = let i = (k + 1 - a) `div` 2 in i * b - (i-1) * a
  | otherwise = 1 + solve (k-1) a b
