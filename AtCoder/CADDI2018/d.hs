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
  as <- replicateM n $ readLn :: IO [Int]

  putStrLn $ if solve as then "first" else "second"

solve :: [Int] -> Bool
solve as = not $ all (\x -> x `mod` 2 == 0) as 