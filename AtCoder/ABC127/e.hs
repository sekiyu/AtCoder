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
solve (n:m:k:_) = otherPatterns `modmul`
  ( foldl' (modadd) 0 [ d * (dxPatterns d) | d <- [1..(n - 1)] ]
  + foldl' (modadd) 0 [ d * (dyPatterns d) | d <- [1..(m - 1)] ]
  )
  where
    otherPatterns = select (n * m - 2) (k - 2)
    dxPatterns d = (n - d) * power m 2
    dyPatterns d = (m - d) * power n 2


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

-- 階乗 n!
factorial :: Integer -> Integer
factorial n = foldr modmul 1 [1..n]

-- n! / m!
fact :: Integer -> Integer -> Integer
fact n m = foldr modmul 1 [(m+1)..n]


-- nCk = n! / (k! * (n - k)!)
-- n個からk個えらぶ
select :: Integer -> Integer -> Integer
select n k = foldl' moddiv (fact n (n - k)) [1..k]
