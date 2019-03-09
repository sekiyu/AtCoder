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
import Data.Bits

main :: IO ()
main = do
  (a:b:_) <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve' a b

solve a b = foldl' xor 0 [a..b]

xors x = foldl' xor 0 [0..x]

solve' a b = xors' b `xor` xors' (a - 1)

xors' x 
  | x `mod` 4 == 0 = x
  | x `mod` 4 == 1 = 1
  | x `mod` 4 == 2 = x + 1
  | x `mod` 4 == 3 = 0


toBinary = reverse . go
  where
    go 0 = []
    go !n = (n `mod` 2):(go (n `div` 2))