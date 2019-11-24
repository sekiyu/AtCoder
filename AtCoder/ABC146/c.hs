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
main = getLine >>= print . solve . map read . words
solve :: [Integer] -> Integer
solve (a:b:x:_) = upperBound buyable (0, 10^9)
  where
    buyable i = price i <= x
    price i = a * i + b * (d i)
    -- d j = 1 + (floor . logBase 10 $ fromIntegral j)
    d j = toInteger . length $ show j

-- upperBound :: (Int -> Bool) -> (Int, Int) -> Int
upperBound predicate bounds = go bounds
  where
    go (l, h) | l + 1 == h = if predicate h then h else l
              | predicate m = go (m, h)
              | otherwise = go (l, m)
      where m = (l + h) `div` 2
