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
  (n:m:_) <- map read . words <$> getLine :: IO [Integer]
  as <- replicateM (fromInteger m) $ readLn :: IO [Integer]
  print $ solve n m as

solve n m as = foldl' modmul 1 $ map f diffs
  where
    sorted = (sort as) ++ [n + 1]
    -- padZero = if head sorted == 1 then sorted else (0:sorted)
    padZero = (-1):sorted
    -- padN = if last sorted == n then padZero else padZero ++ [fromIntegral n + 1]
    diffs = zipWith (\x y -> x - y - 1) (drop 1 padZero) padZero
    -- arr = listArray (1, n) as
    -- dp = listArray (1, n) $ map f [1..n]
    c = listArray (1, n) $ map f [1..n]
    f 0 = 0
    f 1 = 1
    f 2 = 1
    f 3 = 2
    f i = c!(i - 1) + c!(i - 2)



divConst = 10^9 + 7 :: Integer
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
