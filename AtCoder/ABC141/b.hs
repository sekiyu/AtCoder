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
main = getLine >>= putStrLn . solve
solve :: String -> String
solve s = if go s then "Yes" else "No"
  where 
    odds = ['R', 'U', 'D']
    evens = ['L', 'U', 'D']
    go (s0:s1:ss) = if s0 `elem` odds && s1 `elem` evens
                    then go ss
                    else False
    go [] = True
    go (s:[]) = s `elem` odds
    
