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
  n <- readLn :: IO Int
  s <- getLine :: IO String
  putStrLn $ solve n s

solve n s = map (proceed n) s
  where
    proceed i c = head . (drop i) . dropWhile (/=c) $ alphabet

alphabet = cycle ['A'..'Z']
