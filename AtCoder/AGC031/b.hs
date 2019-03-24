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
  n <- readLn
  cs <- replicateM n $ readLn :: IO [Int]
  print $ solve n cs

solve :: Int -> [Int] -> Integer
solve n cs = (dp ! n) + 1
  where
    carr :: Array Int Int
    carr = listArray (1, n) cs
    dp :: Array Int Integer
    dp = runSTArray $ do
      sums <- newArray (0, 200000) 0 :: ST s (STArray s Int Integer)
      dp <- newArray (1, n) 0 :: ST s (STArray s Int Integer)
      forM_ [2..n] $ \i -> do
        prev <- readArray dp (i-1)
        increment <- readArray sums (carr!i)
        writeArray dp i $ if carr!(i-1) /= carr!i
          then prev `modadd` increment
          else prev
        
        -- current <- readArray sums (carr!i-1)
        -- sumSoFar <- sum <$> getElems sums
        if carr!(i-1) /= carr!i
          then writeArray sums (carr!(i-1)) (prev `modadd` 1)
          else return ()
      return dp

divConst = 10^9 + 7 :: Integer
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
  