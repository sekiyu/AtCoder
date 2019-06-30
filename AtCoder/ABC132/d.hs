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
main = getLine >>= solve . map read . words
solve :: [Int] -> IO ()
solve (n:k:_) = forM_ [1..k] $ print . f
  -- forM_ [1..k] $ print . f
  -- print facts
  where
    facts = listArray (0, n) $ scanl' modmul 1 [1..(toInteger n)]
    select l m 
      | l < m = 0
      | otherwise = (facts!l) `moddiv` ((facts!m) `modmul` (facts!(l - m)))
    f i = (select (n - k + 1) i) `modmul` (select (k - 1) (i - 1))

    -- c = listArray ((0,0), (n, k)) $ [ g i j | i<-[0..n], j<-[0..k]]
    -- g 0 _ = 1
    -- g _ 0 = 1
    -- g i j = select (toInteger i) (toInteger j) 
    -- f i = (c!(n - k + 1, i)) `modmul` (c!(k - 1, i - 1))
    -- num = map (\i -> select (n - k + i) i) [1..k]
    -- sums = scanl' modadd 0 num
    -- counts = zipWith modsub num sums
    -- counts = map (\i -> select (n - k + 1) i) [1..k]
    -- blues = map (\i -> select (k - 1) (i - 1)) [1..k]
    -- memo = listArray (1, k) $ map f [1..k]
    -- f i = (select (n - k + i) i  `modsub`  ) `modmul` (select (k - 1) (i - 1))


-- solve (n:k:_) = forM_ [1..k] $ \i ->
--   print $ (select (n - k + i) i) `modmul` (select (k - 1) (i - 1))


divConst = 10^9 + 7 :: Integer
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst
