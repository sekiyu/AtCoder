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
  putStrLn $ solve n

solve :: Int -> String
solve 3 = "YES"
solve 5 = "YES"
solve 7 = "YES"
solve _ = "NO"
