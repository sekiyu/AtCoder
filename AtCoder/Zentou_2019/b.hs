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
  n <- readLn :: IO Int
  ss <- map read . words <$> getLine :: IO [Integer]
  print $ solve n ss

solve n (d:ds)
  | d /= 0 = 0
  | null ds = 1
  | mn == 0 = 0
  | mx == 1 = 1
  | Map.notMember 1 cmap = 0
  | otherwise = go 1 [2..mx] -- foldl' f 1 [2..mx]
    where
      mx = maximum ds
      mn = minimum ds
      cmap = toCountMap ds
      go acc [] = acc
      go acc (ni:ns) 
        | ni > (fromIntegral n) = acc
        | otherwise = case Map.lookup ni cmap of
            Nothing -> 0
            Just cni -> go (acc `modmul` (power cniPrev cni)) ns
          where
            cniPrev = fromIntegral $ cmap Map.! (ni - 1)

      f acc ni = 
        case Map.lookup ni cmap of
          Nothing -> 0
          Just cni -> acc `modmul` (power cniPrev cni)
        where
          cniPrev = fromIntegral $ cmap Map.! (ni - 1)

toCountMap :: (Ord k) => [k] -> Map.Map k Int
toCountMap = Map.fromListWith (+) . flip zip (repeat 1)

divConst = 998244353 :: Integer
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst
