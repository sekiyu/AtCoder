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
-- import Data.Array
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import Data.Char

main :: IO ()
main = getLine >>= print . solve

solve = (!5) . foldl' f initial . zip [0..] . reverse 
  where
    initial = listArray (0, 12) $ 1:replicate 12 0
    f :: Array Int Int -> (Int, Char) -> Array Int Int
    f prev (i, c)
      | c == '?'  = runSTArray $ do
        arr <- newArray (0, 12) 0
        forM_ [0..9] $ \i -> do
          forM_ [0..12] $ \j -> do
            let d = (i * tens + j) `mod` 13
            v <- readArray arr d
            writeArray arr d $ v `modadd` (prev!j)
        return arr
      | otherwise = listArray (0, 12) $ map g [0..12]
        where
          si = digitToInt c
          tens = power 10 i
          rem = (si * tens) `mod` 13
          g j = prev ! ((j - rem) `mod` 13)

divConst = 10^9 + 7
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))

power x y
  | y == 0 = 1
  | y == 1 = x `mod` 13
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` 13
  | otherwise = (power x (y `div` 2))^2 * x `mod` 13
          

    
