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
  as <- replicateM n $ readLn :: IO [Int]
  putStr . unlines . map show $ solve n as

solve n as
  | mx == mx2 = replicate n mx
  | otherwise = [ if a == mx then mx2 else mx | a <- as]
  where
    mx = maximum as
    mx2 = maximum $ delete mx as

