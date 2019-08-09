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
import Data.Char


main :: IO ()
main = do
  k <- readLn :: IO Int
  (x:y:_) <- map read . words <$> getLine :: IO [Int]
  print $ solve k x y

solve k x y = 