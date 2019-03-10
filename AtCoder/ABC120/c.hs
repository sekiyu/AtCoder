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

main :: IO ()
main = do
  s <- getLine 
  print $ solve s

solve str = go [] str
  where
    go [] (s:ss) = go [s] ss
    go ts [] = length str - length ts
    go (t:ts) (s:ss) = if t == s 
                       then go (s:t:ts) ss
                       else go ts ss