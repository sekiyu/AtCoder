-- ABC123 A
{-# LANGUAGE BangPatterns #-}
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
-- import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST

main :: IO ()
main = do
  ss <- replicateM 6 $ readLn :: IO [Int]
  putStrLn $ if solve ss then "Yay!" else ":("

solve ss = k >= mx - mn
  where 
    k = last ss
    as = init ss
    mx = maximum as
    mn = minimum as 
