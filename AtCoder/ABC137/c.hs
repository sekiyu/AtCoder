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
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  n <- readLn :: IO Int
  -- ss <- replicateM n getLine :: IO [String]
  ss <- map (sort . B.unpack) <$> replicateM n B.getLine :: IO [String]
  print $ solve n ss

-- solve :: Int -> [String] -> Int
solve n ss = sum . map fac2 . Map.elems . toCountMap $ ss
  where
    fac2 x = x * (x-1) `div` 2 :: Integer

toCountMap :: (Ord k, Integral a) => [k] -> Map.Map k a
toCountMap xs = Map.fromListWith (+) $ zip xs (repeat 1)
