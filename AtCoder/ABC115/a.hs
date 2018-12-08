-- ABC115 A
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

solve n
  | n == 25 = "Christmas" 
  | n == 24 = "Christmas Eve"
  | n == 23 = "Christmas Eve Eve" 
  | n == 22 =  "Christmas Eve Eve Eve" 
