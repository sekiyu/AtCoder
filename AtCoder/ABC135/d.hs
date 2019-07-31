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
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

main :: IO ()
main = B.getLine >>= print . solve . B.unpack

solve = (!5) . foldl' f initial . zip [0..] . reverse 
  where
    initial = listArray (0, 12) $ 1:replicate 12 0
    f :: UArray Int Int -> (Int, Char) -> UArray Int Int
    f prev (!i, !c)
      | c == '?'  = runSTUArray $ do
        arr <- newArray (0, 12) 0
        forM_ [0..9] $ \j -> do
          forM_ [0..12] $ \k -> do
            let d = (j * tens + k) `mod` 13
            v <- readArray arr d
            writeArray arr d $ v `modadd` (prev!k)
        return arr
      | otherwise = listArray (0, 12) $ map (g si) [0..12]
        where
          si = digitToInt c
          tens = power 10 i
          g j k = prev ! ((k - j * tens) `mod` 13)

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
          

    
