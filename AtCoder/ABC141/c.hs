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

main :: IO ()
main = do
  (n:k:q:_) <- map read . words <$> getLine :: IO [Int]
  as <- replicateM q $ readLn :: IO [Int]
  putStr . unlines  $ solve n k q as

solve n k q as = map (\p -> if k - q + p > 0 then "Yes" else "No") $ elems ans
  where

    ans :: UArray Int Int 
    ans = runSTUArray $ do
      c <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
      forM_ as $ \a -> do
        prev <- readArray c a
        writeArray c a $ prev + 1
      return c
