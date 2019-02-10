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
  abs <- replicateM 3 $ map read . words <$> getLine :: IO [[Int]]
  putStrLn $ solve abs

solve abs = if foldr (||) False [ all (elem i) abs | i <- [1..4] ]
            then "NO"
            else "YES"